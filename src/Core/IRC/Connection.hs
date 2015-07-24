{-# LANGUAGE OverloadedStrings #-}
module Core.IRC.Connection where

import qualified Network as N
import Core.IRC.Types
import Core.IRC.Proto
import Core.IRC.Config

import qualified Data.Map as M
import qualified Data.String.Utils as U
import Data.Configurator
import Data.Configurator.Types
import Data.Text hiding (reverse, dropWhile, null)
import Data.Maybe

import System.IO (hSetBuffering, BufferMode(NoBuffering), hGetLine, hClose)

import Control.Lens
import Control.Monad.State
import Control.Concurrent

start :: [Message -> IRC ()] -> IO ()
start eventListeners = do
  conf <- loadConfig
  servers <- require conf "servers" :: IO [String]
  spawnThreads servers conf eventListeners

spawnThreads :: [String] -> Config -> [Message -> IRC ()] -> IO ()
spawnThreads servers conf eventListeners = do
    children' <- forM servers spawnThread
    forM_ children' takeMVar
  where
    spawnThread server = do
      m <- newEmptyMVar
      _ <- forkThread server m
      return m
    forkThread server m = forkFinally (runThread server) $ \eith -> case eith of
      Left e  -> print e >> threadDelay 10000 >> putStrLn ("Reconnecting " ++ server) >> void (forkThread server m)
      Right _ -> putMVar m ()
    runThread server = do
      irc <- connectTo server conf eventListeners
      evalStateT run irc

connectTo :: String -> Config -> [Message -> IRC ()] -> IO Irc
connectTo server conf eventListeners = do
  addr  <- require conf $ pack (server ++ ".server") :: IO String
  port  <- require conf $ pack (server ++ ".port") :: IO Int
  nick' <- require conf $ pack (server ++ ".nick") :: IO String
  h     <- N.connectTo addr (N.PortNumber $ fromIntegral port)
  hSetBuffering h NoBuffering
  return $ Irc h eventListeners conf server nick' M.empty []

run :: IRC ()
run = do
  nick' <- fromJust <$> lookupServerConfig "nick"
  write $ "NICK " ++ nick'
  write $ "USER " ++ nick' ++ " 0 * :" ++ nick'
  listen

listen :: IRC ()
listen = forever $ do
    h <- use socket
    -- Handle queue first
    queue' <- use eventQueue
    unless (null queue') $ do
      eventQueue .= []
      handleEvents queue'
    -- Handle input
    s <- liftIO $ U.strip <$> hGetLine h
    liftIO $ print s
    msg <- fromRawMessage (parseCommand s)
    handleEvents (return msg)
  where
    handleEvents queue' = do
      eventListeners <- use listeners
      sequence_ $ eventListeners <*> queue'

disconnect :: IRC ()
disconnect = do
  h <- use socket
  liftIO $ hClose h
