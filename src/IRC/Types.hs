module IRC.Types where

import System.IO (Handle)
import Control.Monad.Reader
import Data.Configurator.Types

type User    = String
type Name    = String
type Channel = String
type Host    = String

type IRC = ReaderT Irc IO
data Irc = Irc { socket     :: Handle
               , listeners  :: [Message -> IRC ()]
               , config     :: Config
               , serverName :: String
               }

data UserCommand = UserCommand { ucommand :: String
                               , middles  :: [String]
                               , trailing :: Maybe String
                               }

data RawMessage = RawMessage { prefix  :: Maybe String
                             , command :: String
                             , params  :: [String]
                             }

data Message = Privmsg Channel User String
             | Ping String
             | Invite User Channel
             | Quit User String
             | Part User Channel String
             | Join User Channel
             | Raw RawMessage
