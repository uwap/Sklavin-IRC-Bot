{-# LANGUAGE OverloadedStrings #-}
module IRC.Connection where

import qualified Network as N
import IRC.Proto
import IRC.Config
import Data.Configurator
import Data.Configurator.Types
import Data.Text
import System.IO (hSetBuffering, BufferMode(NoBuffering), hGetLine, hClose)
import Text.Printf (hPrintf, printf)
import Control.Monad
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Control.Concurrent

start :: (Message -> IRC ()) -> IO ()
start eventListener = do
  conf <- loadConfig
  servers <- require conf "servers" :: IO [String]
  spawnThreads servers conf eventListener

spawnThreads :: [String] -> Config -> (Message -> IRC ()) -> IO ()
spawnThreads servers conf eventListener = do
    children <- forM servers $ \server -> do
      m <- newEmptyMVar
      _ <- forkFinally (spawnThread server) $
        either (\e -> print e >> putMVar m ()) (const $ putMVar m ())
      return m
    forM_ children takeMVar
  where
    spawnThread server = do
      irc <- connectTo server conf eventListener
      runReaderT run irc

connectTo :: String -> Config -> (Message -> IRC ()) -> IO Irc
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

run :: IRC ()
run = do
  server <- asks serverName
  conf <- asks config
  nick <- liftIO $ require conf $ pack (server ++ ".nick")
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
