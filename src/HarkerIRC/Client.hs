{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             DeriveDataTypeable #-}

module HarkerIRC.Client where

import Control.Arrow (second)
import Control.Applicative
import Control.Concurrent (ThreadId, myThreadId, forkFinally)
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Typeable
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import HarkerIRC.Types
import Network
import System.Directory
import System.IO

data HarkerClientData = HarkerClientData
    { hcdMessage  :: Maybe IRCInPrivMsg
    , hcdSocket   :: Maybe Socket
    , hcdHandle   :: Maybe Handle
    , hcdMsgQueue :: [String]
    }

harkerClientDataNull = HarkerClientData Nothing Nothing Nothing []

data QuitException = QuitException String
    deriving (Typeable, Show)
instance Exception QuitException

class (Functor m, Monad m, MonadIO m) => HarkerClientMonad m where
    clientLift :: HarkerClient a -> m a
    getSocket  :: m (Maybe Socket)
    getHandle  :: m (Maybe Handle)
    getIRCMsg  :: m (Maybe IRCInPrivMsg)
    getMUser   :: m (Maybe User)
    getUser    :: m User
    getMNick   :: m (Maybe Nick)
    getNick    :: m Nick
    getMMyNick :: m (Maybe Nick)
    getMyNick  :: m Nick
    getMChan   :: m (Maybe Chan)
    getChan    :: m Chan
    getMMsg    :: m (Maybe Message)
    getMsg     :: m Message
    getMAuth   :: m (Maybe Bool)
    getAuth    :: m Bool

    setSocket  :: Socket       -> m ()
    setHandle  :: Handle       -> m ()
    setIRCMsg  :: IRCInPrivMsg -> m ()

    getMsgQueue :: m [String]
    modMsgQueue :: ([String] -> [String]) -> m ()
    getWho      :: Nick -> m (Maybe User)

    getSocket  = clientLift getSocket
    getHandle  = clientLift getHandle
    getIRCMsg  = clientLift getIRCMsg
    getMUser   = clientLift getMUser
    getUser    = fmap (fromMaybe "") getMUser
    getMNick   = clientLift getMNick
    getNick    = fmap (fromMaybe "") getMNick
    getMMyNick = clientLift getMMyNick
    getMyNick  = fmap (fromMaybe "") getMMyNick
    getMChan   = clientLift getMChan
    getChan    = fmap (fromMaybe "" ) getMChan
    getMMsg    = clientLift getMMsg
    getMsg     = fmap (fromMaybe "") getMMsg
    getMAuth   = clientLift getMAuth
    getAuth    = fmap (fromMaybe False) getMAuth

    setSocket = clientLift . setSocket
    setHandle = clientLift . setHandle
    setIRCMsg = clientLift . setIRCMsg

    getMsgQueue = clientLift getMsgQueue
    modMsgQueue = clientLift . modMsgQueue
    getWho      = clientLift . getWho

newtype HarkerClientT m a = HarkerClientT (StateT HarkerClientData m a)
    deriving (Monad, Functor, MonadTrans)

instance (MonadIO m) => MonadIO (HarkerClientT m) where
    liftIO = HarkerClientT . liftIO 

instance (Monad m) => MonadState HarkerClientData (HarkerClientT m) where
    get   = HarkerClientT get
    put   = HarkerClientT . put
    state = HarkerClientT . state

instance (Functor m, Monad m, MonadIO m) => 
         HarkerClientMonad (HarkerClientT m) where
    clientLift = undefined
    getSocket  = gets hcdSocket
    getHandle  = gets hcdHandle
    getIRCMsg  = gets hcdMessage
    getMUser   = gets (fmap ircUser   . hcdMessage)
    getMNick   = gets (fmap ircNick   . hcdMessage)
    getMMyNick = gets (fmap ircMyNick . hcdMessage)
    getMChan   = gets (fmap ircChan   . hcdMessage)
    getMMsg    = gets (fmap ircMsg    . hcdMessage)
    getMAuth   = gets (fmap ircAuth   . hcdMessage)

    setSocket x = modify (\m -> m { hcdSocket  = Just x })
    setHandle x = modify (\m -> m { hcdHandle  = Just x })
    setIRCMsg x = modify (\m -> m { hcdMessage = Just x })

    getMsgQueue   = gets hcdMsgQueue
    modMsgQueue f = modify (\s -> s { hcdMsgQueue = f $ hcdMsgQueue s })
    getWho        = doGetWho

type HarkerClient a = HarkerClientT IO a

runHarkerClientT :: (Monad m) => HarkerClientT m a -> m a
runHarkerClientT (HarkerClientT s) = evalStateT s harkerClientDataNull

runHarkerClient :: HarkerClient a -> IO a
runHarkerClient (HarkerClientT s) = evalStateT s harkerClientDataNull

runPlugin :: (HarkerClientMonad m) => String -> String 
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

acceptfunc :: (HarkerClientMonad m) => m () -> (m () -> IO ()) 
           -> Maybe Socket -> IO ()
acceptfunc f run (Just sock) = do
    tid <- myThreadId
    loopfunc $ do
        putStrLn "ready to accpet a new connection"
        bracket (accept sock) 
            (\(h,_,_) -> hIsOpen h >>= \isOpen -> when isOpen $ hClose h)
            (\(h, _, _) -> putStrLn "accepted connection" 
                           >> forkfunc h tid f run)
            
acceptfunc _ _   _           = return ()

forkfunc :: (HarkerClientMonad m) => Handle -> ThreadId 
         -> m () -> (m () -> IO ()) -> IO ()
forkfunc h tid f run =
    run $ do
        setHandle h
        handleRequest tid f

handleRequest :: (HarkerClientMonad m) => ThreadId -> m () -> m ()
handleRequest tid f = do
    mh <- getHandle 
    case mh of
        Just h -> loopfunc $ do
            l <- getMsgQueue
            modMsgQueue (const [])
            mircmsg <- if hasQuit l then return Nothing
                                    else liftIO $ buildIRCMsg l h
            case mircmsg of
                Nothing     -> liftIO (shutdown tid h)
                Just ircmsg -> setIRCMsg ircmsg >> f
        _      -> liftIO $ throwTo tid (QuitException "no handle")

buildIRCMsg :: [String] -> Handle -> IO (Maybe IRCInPrivMsg)
buildIRCMsg l = fmap (fmap (fromList . (l ++))) . buildIRCMsg'
  where
    buildIRCMsg' :: Handle -> IO (Maybe [String])
    buildIRCMsg' h = do
        l <- hGetLine h
        if l == "action: quit" then return Nothing
        else if l == "-"       then return $ Just []
                               else fmap (l:) <$> buildIRCMsg' h

hasQuit :: [String] -> Bool
hasQuit = isJust . find (== "action: quit")

shutdown :: ThreadId -> Handle -> IO ()
shutdown tid h = do
    putStrLn "recieved exit command"
    hClose h
    throwTo tid (QuitException "done")

sendReply :: (HarkerClientMonad m) => String -> m ()
sendReply msg = do
    mnick <- getMNick
    mchan <- getMChan
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


ifauth :: (HarkerClientMonad m) => m () -> m ()
ifauth f = do
    auth <- getAuth
    if auth then f else sendReply "you are not authenticated for that"

doGetWho :: (HarkerClientMonad m) => Nick -> m (Maybe User)
doGetWho n = do
    mh <- getHandle
    case mh of
        Just h -> do
            liftIO $ hPutStrLn h "%who"
            l <- buildWhoList h
            case find (\(n', _) -> n' == n) l of
                Just (_, u) -> return $ Just u
                _           -> return Nothing
        _      -> liftIO (putStrLn "no handle") >> return Nothing


buildWhoList :: (HarkerClientMonad m) => Handle -> m [(String, String)]
buildWhoList h = do
    l <- liftIO $ hGetLine h
    if l == "%wholist" then liftIO $ readWhoList h
    else modMsgQueue (l:) >> buildWhoList h

readWhoList :: Handle -> IO [(String, String)]
readWhoList h = do
    l <- hGetLine h
    if l == "%endwholist" then return []
                          else (splitInput l:) <$> readWhoList h
  where
    splitInput :: String -> (String, String)
    splitInput = second (tail . tail) . break (== ':')
