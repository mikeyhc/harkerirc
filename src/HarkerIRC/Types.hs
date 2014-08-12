{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module HarkerIRC.Types 
    -- type definitions
    ( Nick
    , User
    , Chan
    , Message
    , InMessageQueue
    , OutMessageQueue
    , RawIRCString

    -- data/newtype definitions
    , IRCInPrivMsg (..)
    , IRCOutPrivMsg (..)
    , IRCSystemMsg (..)
    , IRCException (..)

    -- instances
    , Listable (..)
    , IRCBasicMessage (..)
    , IRCAdvancedMessage (..)
    ) where

import Control.Exception
import Data.Char
import Data.List
import Data.Typeable

class Listable a where
    toList   :: a -> [String]
    fromList :: [String] -> a

class IRCBasicMessage a where
    ircNick :: a -> String
    ircChan :: a -> String
    ircMsg  :: a -> String

class IRCAdvancedMessage a where
    ircUser :: a -> String
    ircAuth :: a -> Bool

type Nick            = String
type User            = String
type Chan            = String
type Message         = String
type InMessageQueue  = [IRCInPrivMsg]
type OutMessageQueue = [IRCOutPrivMsg]
type RawIRCString    = String

data IRCInPrivMsg = IRCInPrivMsg
                  { _inircnick :: Nick
                  , _inircuser :: User
                  , _inircauth :: Bool
                  , _inircchan :: Chan
                  , _inircmsg  :: Message
                  }

instance IRCBasicMessage IRCInPrivMsg where
    ircNick = _inircnick
    ircChan = _inircchan
    ircMsg  = _inircmsg

instance IRCAdvancedMessage IRCInPrivMsg where
    ircUser = _inircuser
    ircAuth = _inircauth

data IRCOutPrivMsg = IRCOutPrivMsg
                   { _outircnick :: Nick
                   , _outircchan :: Chan
                   , _outircmsg  :: Message
                   }

instance IRCBasicMessage IRCOutPrivMsg where
    ircNick = _outircnick
    ircChan = _outircchan
    ircMsg  = _outircmsg

newtype IRCSystemMsg = IRCSystemMsg
                     { getIRCSysMsg :: String
                     }

nullIRCInPrivMsg :: IRCInPrivMsg
nullIRCInPrivMsg = IRCInPrivMsg "" "" False "" ""

nullIRCOutPrivMsg :: IRCOutPrivMsg
nullIRCOutPrivMsg = IRCOutPrivMsg "" "" ""

instance Listable IRCInPrivMsg where
    toList (IRCInPrivMsg n u a c m) =
        [ "nick: " ++ n
        , "user: " ++ u
        , "auth: " ++ show a
        , "chan: " ++ c
        , "msg: "  ++ m
        ]
    
    fromList = foldl mkmessage nullIRCInPrivMsg
        where
            strlower = map toLower
            mkmessage m s
                | "nick: " `isPrefixOf`     s = m { _inircnick = drop 6 s }
                | "user: " `isPrefixOf`     s = m { _inircuser = drop 6 s }
                | "auth: true"  == strlower s = m { _inircauth = True     }
                | "auth: false" == strlower s = m { _inircauth = False    }
                | "chan: " `isPrefixOf`     s = m { _inircchan = drop 6 s }
                | "msg: "  `isPrefixOf`     s = m { _inircmsg  = drop 5 s }
                | otherwise                   = throw $ mpuException s

instance Listable IRCOutPrivMsg where
    toList (IRCOutPrivMsg n c m) =
        [ "nick: " ++ n
        , "chan: " ++ c
        , "msg: "  ++ m
        ]

    fromList = foldl mkmessage nullIRCOutPrivMsg
        where
            mkmessage m s
                | "nick: " `isPrefixOf` s = m { _outircnick = drop 6 s }
                | "chan: " `isPrefixOf` s = m { _outircchan = drop 6 s }
                | "msg: "  `isPrefixOf` s = m { _outircmsg  = drop 5 s }
                | otherwise               = throw $ mpuException s


data IRCException = IRCMessageParseException String
    deriving (Typeable, Show)
instance Exception IRCException

mpuException :: String -> IRCException
mpuException = IRCMessageParseException . (++) "Unrecognized token: "
