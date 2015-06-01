module IRC.Types where

import System.IO (Handle)
import Control.Monad.Reader
import Data.Configurator.Types

type Nick    = String
type Name    = String
type Channel = String
type Host    = String

type IRC = ReaderT Irc IO
data Irc = Irc { socket     :: Handle
               , listener   :: RawMessage -> IRC ()
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
