module HarkerIRC.Types where

type Nick = String
type User = String
type Chan = String
type Message = String

data IRCMessage = IRCMessage { ircnick :: Nick
                             , ircuser :: User
                             , ircchan :: Chan
                             , ircmsg  :: Message
                             }
