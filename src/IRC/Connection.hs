module IRC.Connection where

import qualified Network as N
import IRC.Proto
import IRC.Config
import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle, hGetLine, hClose)
import Text.Printf (hPrintf)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)

type IRC = ReaderT Irc IO
data Irc = Irc { socket :: Handle, config :: Config }

start :: Config -> IO ()
start config = do
  irc <- connectTo config
  runReaderT run irc

connectTo :: Config -> IO Irc
connectTo config = do
  h <- N.connectTo (server config) (N.PortNumber (fromIntegral (port config)))
  hSetBuffering h NoBuffering
  return $ Irc h config

write :: String -> IRC ()
write s = do
  h <- asks socket
  liftIO $ hPrintf h "%s\r\n" s

run :: IRC ()
run = do
  conf <- asks config
  write $ "NICK " ++ nick conf
  write $ "USER " ++ nick conf ++ " 0 * :" ++ nick conf
  listen

listen :: IRC ()
listen = forever $ do
  h <- asks socket
  s <- liftIO $ hGetLine h
  liftIO $ putStrLn s

disconnect :: Maybe String -> IRC ()
disconnect m = do
  write $ ucQuit m
  h <- asks socket
  liftIO $ hClose h
