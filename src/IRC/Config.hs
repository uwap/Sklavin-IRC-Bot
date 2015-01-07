module IRC.Config where

import IRC.Proto
import System.IO (Handle)
import Control.Monad.Reader (ReaderT)

type IRC = ReaderT Irc IO
data Irc = Irc { socket :: Handle, config :: Config }

data Config = Config { server   :: String
                     , port     :: Int
                     , nick     :: String
                     , listener :: Message -> IRC ()
                     }
