{-# LANGUAGE OverloadedStrings #-}
module IRC.Connection where

import qualified Network as N
import IRC.Proto
import IRC.Config
import Data.Configurator
import Data.Configurator.Types
import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle, hGetLine, hClose)
import Text.Printf (hPrintf, printf)
import Control.Monad (forever)
import Control.Monad.Reader (asks, liftIO, runReaderT)

start :: (Message -> IRC ()) -> IO ()
start eventListener = do
  config <- loadConfig
  irc <- connectTo config eventListener
  runReaderT run irc

connectTo :: Config -> (Message -> IRC ()) -> IO Irc
connectTo config eventListener = do
  server <- require config "server"
  port <- require config "port" :: IO Int
  h <- N.connectTo server (N.PortNumber $ fromIntegral port)
  hSetBuffering h NoBuffering
  return $ Irc h eventListener config

write :: String -> IRC ()
write s = do
  h <- asks socket
  liftIO $ do
    hPrintf h "%s\r\n" s
    printf "» %s\r\n" s

run :: IRC ()
run = do
  conf <- asks config
  nick <- liftIO $ require conf "nick"
  write $ "NICK " ++ nick
  write $ "USER " ++ nick ++ " 0 * :" ++ nick
  listen

listen :: IRC ()
listen = forever $ do
  h <- asks socket
  s <- liftIO $ hGetLine h
  liftIO $ putStrLn s
  eventListener <- asks listener
  eventListener $ parseCommand s

disconnect :: Maybe String -> IRC ()
disconnect m = do
  write $ ucQuit m
  h <- asks socket
  liftIO $ hClose h
