{-# LANGUAGE OverloadedStrings #-}
module IRC.Connection where

import qualified Network as N
import IRC.Proto
import IRC.Config
import Data.Configurator
import Data.Configurator.Types
import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle, hGetLine, hClose)
import Text.Printf (hPrintf, printf)
import Control.Monad
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Control.Concurrent

start :: (Message -> IRC ()) -> IO ()
start eventListener = do
  config <- loadConfig
  servers <- require config "servers" :: IO [String]
  spawnThreads servers config eventListener

spawnThreads :: [String] -> Config -> (Message -> IRC ()) -> IO ()
spawnThreads servers config eventListener = do
    children <- forM servers $ \server -> do
      m <- newEmptyMVar
      forkFinally (spawnThread m server config eventListener) $ const $ putMVar m ()
      return m
    forM_ children takeMVar
  where
    spawnThread m server config eventListener = do
      irc <- connectTo server config eventListener
      runReaderT run irc

connectTo :: String -> Config -> (Message -> IRC ()) -> IO Irc
connectTo server config eventListener = do
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
