{-# LANGUAGE DeriveDataTypeable #-}

module HarkerIRC.Types where

import Control.Exception
import Data.List
import Data.Typeable

type Nick = String
type User = String
type Chan = String
type Message = String

data IRCMessage = IRCMessage { ircnick :: Nick
                             , ircuser :: User
                             , ircauth :: Bool
                             , ircchan :: Chan
                             , ircmsg  :: Message
                             }
nullIRCMessage = IRCMessage "" "" False "" ""

data IRCException = IRCMessageParseException String
    deriving (Typeable, Show)

instance Exception IRCException

ircMessageToList :: IRCMessage -> [String]
ircMessageToList (IRCMessage n u a c m) =
    [ "nick: " ++ n
    , "user: " ++ c
    , "auth: " ++ show a
    , "chan: " ++ c
    , "msg: "  ++ m
    ]

mpuException :: String -> IRCException
mpuException = IRCMessageParseException . (++) "Unrecognized token: "

ircMessageFromList :: [String] -> IRCMessage
ircMessageFromList = foldl mkmessage nullIRCMessage
    where
        mkmessage :: IRCMessage -> String -> IRCMessage
        mkmessage m s
            | "nick: " `isPrefixOf` s = m { ircnick = drop 6 s }
            | "user: " `isPrefixOf` s = m { ircuser = drop 6 s }
            | "auth: true"  ==      s = m { ircauth = True     }
            | "auth: false" ==      s = m { ircauth = False    }
            | "chan: " `isPrefixOf` s = m { ircchan = drop 6 s }
            | "msg: "  `isPrefixOf` s = m { ircmsg  = drop 5 s }
            | otherwise               = throw $ mpuException s
