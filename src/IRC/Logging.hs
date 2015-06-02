module IRC.Logging where

import IRC.Types
import IRC.Config

import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Reader

import Data.Time.LocalTime
import Data.Maybe

logFile :: String -> IRC (Maybe FilePath)
logFile chan = do
    serverLogPath <- lookupServerConfig "logs"
    let filePath = serverLogPath >>= \path -> return (path </> chan <.> "log")
    case filePath of
      Just path -> liftIO $ liftM (fromBool path) $ doesDirectoryExist (fromJust serverLogPath)
      Nothing -> return Nothing
  where
    fromBool a True  = Just a
    fromBool _ False = Nothing

logMessage :: Message -> IRC ()
logMessage msg@(Privmsg chan user message) = do
    time <- liftIO getZonedTime
    case msg of
      Privmsg chan user message -> logChannelMessage chan ("[" ++ show time ++ "] <" ++ user ++ "> " ++ message ++ "\n")
      _                         -> return ()

logChannelMessage :: Channel -> String -> IRC ()
logChannelMessage channel message = do
  filepath <- logFile chan
  liftIO $ case filepath of
    Just path -> appendFile path message
    Nothing   -> return ()
