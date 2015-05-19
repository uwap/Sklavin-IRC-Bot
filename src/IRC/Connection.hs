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
import Text.Printf (hPrintf, printf)

import Control.Monad
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Control.Concurrent

data ThreadReturn = Restart | Finish

start :: (RawMessage -> IRC ()) -> IO ()
start eventListener = do
  conf <- loadConfig
  servers <- require conf "servers" :: IO [String]
  spawnThreads servers conf eventListener

spawnThreads :: [String] -> Config -> (RawMessage -> IRC ()) -> IO ()
spawnThreads servers conf eventListener = do
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
      irc <- connectTo server conf eventListener
      runReaderT run irc

connectTo :: String -> Config -> (RawMessage -> IRC ()) -> IO Irc
connectTo server conf eventListener = do
  addr <- require conf $ pack (server ++ ".server") :: IO String
  port <- require conf $ pack (server ++ ".port") :: IO Int
  h <- N.connectTo addr (N.PortNumber $ fromIntegral port)
  hSetBuffering h NoBuffering
  return $ Irc h eventListener conf server

write :: String -> IRC ()
write s = do
  h <- asks socket
  liftIO $ do
    hPrintf h "%s\r\n" s
    printf "» %s\r\n" s

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
  eventListener <- asks listener
  eventListener $ parseCommand s
  return Restart

disconnect :: Maybe String -> IRC ()
disconnect m = do
  write . evaluateUserCommand $ quit m
  h <- asks socket
  liftIO $ hClose h
