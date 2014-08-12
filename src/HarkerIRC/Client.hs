{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             DeriveDataTypeable #-}

module HarkerIRC.Client where

import Control.Applicative
import Control.Concurrent (ThreadId, myThreadId, forkFinally)
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Typeable
import HarkerIRC.Types
import Network
import System.Directory
import System.IO

data HarkerClientData = HarkerClientData
    { hcdMessage :: Maybe IRCInPrivMsg
    , hcdSocket  :: Maybe Socket
    , hcdHandle  :: Maybe Handle
    }

harkerClientDataNull = HarkerClientData Nothing Nothing Nothing

data QuitException = QuitException String
    deriving (Typeable, Show)
instance Exception QuitException

class HarkerClientMonad m where
    getSocket :: m (Maybe Socket)
    getHandle :: m (Maybe Handle)
    getIRCMsg :: m (Maybe IRCInPrivMsg)
    getUser   :: m (Maybe User)
    getNick   :: m (Maybe Nick)
    getChan   :: m (Maybe Chan)
    getMMsg   :: m (Maybe Message)
    getMsg    :: m Message
    getMAuth  :: m (Maybe Bool)
    getAuth   :: m Bool

    setSocket :: Socket       -> m ()
    setHandle :: Handle       -> m ()
    setIRCMsg :: IRCInPrivMsg -> m ()

newtype HarkerClientT m a = HarkerClientT (StateT HarkerClientData m a)
    deriving (Monad, Functor, MonadTrans)

instance (MonadIO m) => MonadIO (HarkerClientT m) where
    liftIO = HarkerClientT . liftIO 

instance (Monad m) => MonadState HarkerClientData (HarkerClientT m) where
    get   = HarkerClientT $ get
    put   = HarkerClientT . put
    state = HarkerClientT . state

instance (Functor m, Monad m) => HarkerClientMonad (HarkerClientT m) where
    getSocket = gets hcdSocket
    getHandle = gets hcdHandle
    getIRCMsg = gets hcdMessage
    getUser   = gets (fmap ircUser . hcdMessage)
    getNick   = gets (fmap ircNick . hcdMessage)
    getChan   = gets (fmap ircChan . hcdMessage)
    getMMsg   = gets (fmap ircMsg  . hcdMessage)
    getMsg    = gets (maybe ""  ircMsg . hcdMessage)
    getMAuth  = gets (fmap ircAuth . hcdMessage)
    getAuth   = gets (maybe False ircAuth . hcdMessage)

    setSocket x = modify (\m -> m { hcdSocket  = Just x })
    setHandle x = modify (\m -> m { hcdHandle  = Just x })
    setIRCMsg x = modify (\m -> m { hcdMessage = Just x })

type HarkerClient a = HarkerClientT IO a

runHarkerClientT :: (Monad m) => HarkerClientT m a -> m a
runHarkerClientT (HarkerClientT s) = evalStateT s harkerClientDataNull

runHarkerClient :: HarkerClient a -> IO a
runHarkerClient (HarkerClientT s) = evalStateT s harkerClientDataNull

runPlugin :: (HarkerClientMonad m, Monad m, MonadIO m) => String -> String 
          -> m () -> (m () -> IO ())-> IO ()
runPlugin n v f run = let sockaddr = "/tmp/." ++ n ++ ".sock"
    in bracket (pluginStartup n v sockaddr) (pluginShutdown sockaddr) 
            (acceptfunc f run)
    

pluginStartup :: String -> String -> String -> IO (Maybe Socket)
pluginStartup n v sockaddr = do
    s <- listenOn (UnixSocket sockaddr)
    h <- register n v sockaddr
    r <- hGetLine h
    if r == "registered" 
        then return (Just s)
        else hPutStrLn stderr ("error: " ++ r) >> return Nothing

pluginShutdown :: String -> Maybe Socket -> IO ()
pluginShutdown sockaddr _       = do
    b <- doesFileExist sockaddr 
    when b $ removeFile sockaddr

acceptfunc :: (HarkerClientMonad m, Monad m, MonadIO m) => m () 
           -> (m () -> IO ()) -> Maybe Socket -> IO ()
acceptfunc f run (Just sock) = do
    tid <- myThreadId
    loopfunc $ do
        putStrLn "ready to accpet a new connection"
        bracket (accept sock) (\(h,_,_) -> hClose h) 
            (\(h, _, _) -> putStrLn "accepted connection" >> 
                (forkfunc h tid f run))
            
acceptfunc _ _   _           = return ()

forkfunc :: (HarkerClientMonad m, Monad m, MonadIO m) => Handle -> ThreadId 
         -> m () -> (m () -> IO ()) -> IO ()
forkfunc h tid f run =
    run $ do
        setHandle h
        handleRequest tid f

handleRequest :: (HarkerClientMonad m, Monad m, MonadIO m) => ThreadId 
              -> m () -> m ()
handleRequest tid f = do
    mh <- getHandle 
    case mh of
        Just h -> loopfunc $ do
            mircmsg <- liftIO $ buildIRCMsg h
            case mircmsg of
                Nothing     -> liftIO (hClose h 
                                      >> throwTo tid (QuitException "done"))
                Just ircmsg -> setIRCMsg ircmsg >> f
        _      -> liftIO $ throwTo tid (QuitException "no handle")

buildIRCMsg :: Handle -> IO (Maybe IRCInPrivMsg)
buildIRCMsg = fmap (fmap fromList) . buildIRCMsg'
    where
        buildIRCMsg' :: Handle -> IO (Maybe [String])
        buildIRCMsg' h = do
            l <- hGetLine h
            if l == "action: quit" then do
                                        putStrLn "recieved exit command"
                                        return Nothing
            else if l == "-"       then return $ Just []
                                   else fmap (fmap (l:)) $ buildIRCMsg' h

sendReply :: (HarkerClientMonad m, Monad m, MonadIO m) => String -> m ()
sendReply msg = do
    mnick <- getNick
    mchan <- getChan
    mh    <- getHandle
    case (mnick, mchan, mh) of
        (Just nick, Just chan, Just h) ->  do
            mapM_ (liftIO . hPutStrLn h) . toList 
                $ IRCOutPrivMsg nick chan msg
            liftIO $ hPutStrLn h "-"
        _                              ->
            liftIO $ putStrLn "no message found"

register :: String -> String -> String -> IO Handle
register n v s = do
    h <- connectTo "localhost" (UnixSocket "/tmp/.harker-server.sock")
    hPutStrLn h $ "name: " ++ n
    hPutStrLn h $ "version: " ++ v
    hPutStrLn h $ "server: " ++ s
    return h

loopfunc a = a >> loopfunc a

quitcatch :: QuitException -> IO ()
quitcatch _ = putStrLn "quiting"

trim [x]    | x == '\n' = []
            | otherwise = [x]
trim (x:xs) = x:trim xs
