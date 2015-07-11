{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module IRC.Types where

import System.IO (Handle)
import Control.Monad.Reader
import Control.Lens.TH
import Data.Configurator.Types
import Data.Map
import Data.IORef

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

type IRC = ReaderT Irc IO
data Irc = Irc { socket     :: Handle
               , listeners  :: [Message -> IRC ()]
               , config     :: Config
               , serverName :: String
               , channelRef :: IORef (Map ChannelName Channel)
               }

data Channel = Channel { channelName :: ChannelName
                       }
makeFields ''Channel

data UserCommand = UserCommand { ucommand :: String
                               , middles  :: [String]
                               , trailing :: Maybe String
                               }

