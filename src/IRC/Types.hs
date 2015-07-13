{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module IRC.Types where

import System.IO (Handle)
import Control.Monad.State
import Control.Lens.TH
import Data.Configurator.Types
import Data.Map

type User        = String
type Name        = String
type ChannelName = String
type Host        = String

data RawMessage = RawMessage { prefix  :: Maybe String
                             , command :: String
                             , params  :: [String]
                             }
                             
data Message = Privmsg User String Channel -- Someone wrote something on some channel
             | Ping String                 -- Received a Ping
             | Invite User Channel         -- The bot was invited by some user to some channel
             | Quit User String            -- Some user quit the server with some message
             | Part User String Channel    -- Some user quit some channel with some message
             | Join User Channel           -- Some user joined some channel
             | Send String Channel         -- The bot wrote some message in some channel
             | Raw RawMessage

data Channel = Channel { channelName  :: ChannelName
                       , channelUsers :: Map User User
                       }

makeFields ''Channel

type IRC = StateT Irc IO
data Irc = Irc { ircSocket     :: Handle
               , ircListeners  :: [Message -> IRC ()]
               , ircConfig     :: Config
               , ircServerName :: String
               , ircNick       :: String
               , ircChannels   :: Map ChannelName Channel
               , ircEventQueue :: [Message]               -- Events that are triggered outside of listen
               }
makeFields ''Irc

data UserCommand = UserCommand { ucommand :: String
                               , middles  :: [String]
                               , trailing :: Maybe String
                               }

