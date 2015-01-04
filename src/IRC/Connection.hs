module IRC.Connection where

import qualified Network as N
import IRC.Proto
import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle, hGetLine, hClose)
import Text.Printf (hPrintf)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)

type IRC = ReaderT Irc IO
data Irc = Irc { socket :: Handle }

start :: Integral a => String -> a -> IO ()
start server port = do
  irc <- connectTo server port
  runReaderT run irc

connectTo :: Integral a => String -> a -> IO Irc
connectTo server port = do
  h <- N.connectTo server (N.PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return $ Irc h

write :: String -> IRC ()
write s = do
  h <- asks socket
  liftIO $ hPrintf h "%s\r\n" s

run :: IRC ()
run = do
  -- connect
  listen

listen :: IRC ()
listen = return ()

disconnect :: Maybe String -> IRC ()
disconnect m = do
  write $ ucQuit m
  h <- asks socket
  liftIO $ hClose h
