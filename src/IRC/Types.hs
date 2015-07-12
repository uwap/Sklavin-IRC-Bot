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
                             
data Message = Privmsg User String Channel
             | Ping String
             | Invite User Channel
             | Quit User String
             | Part User String Channel
             | Join User Channel
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
               , ircChannels   :: Map ChannelName Channel
               }
makeFields ''Irc

data UserCommand = UserCommand { ucommand :: String
                               , middles  :: [String]
                               , trailing :: Maybe String
                               }

