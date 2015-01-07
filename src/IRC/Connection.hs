module IRC.Connection where

import qualified Network as N
import IRC.Proto
import IRC.Config
import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle, hGetLine, hClose)
import Text.Printf (hPrintf, printf)
import Control.Monad (forever)
import Control.Monad.Reader (asks, liftIO, runReaderT)

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
  liftIO $ printf "» %s\r\n" s

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
  conf <- asks config
  listener conf $ parseCommand s

disconnect :: Maybe String -> IRC ()
disconnect m = do
  write $ ucQuit m
  h <- asks socket
  liftIO $ hClose h
