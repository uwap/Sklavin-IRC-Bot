{-# LANGUAGE OverloadedStrings #-}
module IRC.Connection where

import qualified Network as N
import IRC.Types
import IRC.Proto
import IRC.Config

import qualified Data.Map as M
import Data.Configurator
import Data.Configurator.Types
import Data.Text hiding (reverse, dropWhile)
import Data.Maybe
import Data.Char
import Data.IORef

import System.IO (hSetBuffering, BufferMode(NoBuffering), hGetLine, hClose)

import Control.Monad
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Control.Concurrent

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
      _ <- forkThread server m
      return m
    forkThread server m = forkFinally (runThread server) $ \eith -> case eith of
      Left e  -> print e >> threadDelay 10000 >> putStrLn ("Reconnecting " ++ server) >> void (forkThread server m)
      Right _ -> putMVar m ()
    runThread server = do
      irc <- connectTo server conf eventListeners
      runReaderT run irc

connectTo :: String -> Config -> [Message -> IRC ()] -> IO Irc
connectTo server conf eventListeners = do
  addr <- require conf $ pack (server ++ ".server") :: IO String
  port <- require conf $ pack (server ++ ".port") :: IO Int
  h <- N.connectTo addr (N.PortNumber $ fromIntegral port)
  hSetBuffering h NoBuffering
  chanRef <- liftIO $ newIORef M.empty
  return $ Irc h eventListeners conf server chanRef

run :: IRC ()
run = do
  nick <- fromJust <$> lookupServerConfig "nick"
  write $ "NICK " ++ nick
  write $ "USER " ++ nick ++ " 0 * :" ++ nick
  listen

listen :: IRC ()
listen = forever $ do
    h <- asks socket
    s <- liftIO $ trim <$> hGetLine h
    liftIO $ putStrLn s
    eventListeners <- asks listeners
    msg <- fromRawMessage (parseCommand s)
    sequence_ $ eventListeners <*> return msg
  where
    trim = reverse . dropWhile isSpace . reverse

disconnect :: IRC ()
disconnect = do
  h <- asks socket
  liftIO $ hClose h
