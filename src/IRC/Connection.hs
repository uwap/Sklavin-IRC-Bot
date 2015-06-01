{-# LANGUAGE OverloadedStrings #-}
module IRC.Connection where

import qualified Network as N
import IRC.Types
import IRC.Proto
import IRC.Config

import Data.Configurator
import Data.Configurator.Types
import Data.Text
import Data.Maybe

import System.IO (hSetBuffering, BufferMode(NoBuffering), hGetLine, hClose)

import Control.Monad
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Control.Concurrent

data ThreadReturn = Restart | Finish

start :: [Message -> IRC ()] -> IO ()
start eventListeners = do
  conf <- loadConfig
  servers <- require conf "servers" :: IO [String]
  spawnThreads servers conf eventListeners

spawnThreads :: [String] -> Config -> [Message -> IRC ()] -> IO ()
spawnThreads servers conf eventListeners = do
    children <- forM servers spawnThread
    forM_ children takeMVar
  where
    spawnThread server = do
      m <- newEmptyMVar
      _ <- forkFinally (runThread server) $ \eith -> case eith of
        Left e -> print e >> void (spawnThread server)
        Right r -> case r of
                     Restart -> void (spawnThread server)
                     Finish  -> putMVar m ()
      return m
    runThread server = do
      irc <- connectTo server conf eventListeners
      runReaderT run irc

connectTo :: String -> Config -> [Message -> IRC ()] -> IO Irc
connectTo server conf eventListeners = do
  addr <- require conf $ pack (server ++ ".server") :: IO String
  port <- require conf $ pack (server ++ ".port") :: IO Int
  h <- N.connectTo addr (N.PortNumber $ fromIntegral port)
  hSetBuffering h NoBuffering
  return $ Irc h eventListeners conf server

run :: IRC ThreadReturn
run = do
  nick <- fromJust <$> lookupServerConfig "nick"
  write $ "NICK " ++ nick
  write $ "USER " ++ nick ++ " 0 * :" ++ nick
  listen

listen :: IRC ThreadReturn
listen = forever $ do
  h <- asks socket
  s <- liftIO $ hGetLine h
  liftIO $ putStrLn s
  eventListeners <- asks listeners
  sequence_ $ eventListeners <*> return (fromRawMessage $ parseCommand s)
  return Restart

disconnect :: IRC ()
disconnect = do
  h <- asks socket
  liftIO $ hClose h
