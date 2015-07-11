module IRC.Logging where

import IRC.Types
import IRC.Config

import System.Directory
import System.FilePath

import Control.Lens hiding ((<.>))
import Control.Monad
import Control.Monad.Reader

import Data.Time.LocalTime
import Data.Maybe

logFile :: Channel -> IRC (Maybe FilePath)
logFile chan = do
    serverLogPath <- lookupServerConfig "logs"
    let filePath = serverLogPath >>= \path -> return (path </> (chan ^. name) <.> "log")
    case filePath of
      Just path -> liftIO $ liftM (fromBool path) $ doesDirectoryExist (fromJust serverLogPath)
      Nothing -> return Nothing
  where
    fromBool a True  = Just a
    fromBool _ False = Nothing

logMessage :: Message -> IRC ()
logMessage msg = do
    time <- liftIO getZonedTime
    case msg of
      Privmsg user message chan -> log' chan time ("<" ++ user ++ "> " ++ message)
      Join user chan            -> log' chan time ("* " ++ user ++ " joined " ++ (chan ^. name))
      Part user message chan    -> log' chan time ("* " ++ user ++ " quit the channel (" ++ message ++ ")")
-- TODO: Log to all channels:
--      Quit user message         -> log chan time ("* " ++ user ++ " quit the server (" ++ message ++ ")")
      _                         -> return ()
  where
    log' chan time str = logChannelMessage chan ("[" ++ show time ++ "] " ++ str ++ "\n")

logChannelMessage :: Channel -> String -> IRC ()
logChannelMessage channel message = do
  filepath <- logFile channel
  liftIO $ case filepath of
    Just path -> appendFile path message
    Nothing   -> return ()
