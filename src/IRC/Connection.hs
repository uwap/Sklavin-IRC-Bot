module IRC.Connection where

import qualified Network as N
import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle, hGetLine)
import Control.Monad.Trans.Reader (ReaderT)

type IRC = ReaderT Irc IO
data Irc = Irc { socket :: Handle }

connectTo :: Integral a => String -> a -> IO Irc
connectTo server port = do
  h <- N.connectTo server (N.PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return $ Irc h
