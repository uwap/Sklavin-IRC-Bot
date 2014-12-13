module IRC.Connection where

import qualified Network as N
import IRC.Proto
import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle, hGetLine)
import Text.Printf (hPrintf)
import Control.Monad.Reader (ReaderT, asks, liftIO)

type IRC = ReaderT Irc IO
data Irc = Irc { socket :: Handle }

connectTo :: Integral a => String -> a -> IO Irc
connectTo server port = do
  h <- N.connectTo server (N.PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return $ Irc h

write :: String -> IRC ()
write s = do
  h <- asks socket
  liftIO $ hPrintf h "%s\r\n" s

disconnect :: Maybe String -> IRC ()
disconnect m = do
  return ()
