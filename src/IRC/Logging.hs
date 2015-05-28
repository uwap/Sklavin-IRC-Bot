module IRC.Logging where

import IRC.Types
import IRC.Config
import IRC.Proto (parseUserHost)

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

logMessage :: RawMessage -> IRC ()
logMessage (RawMessage (Just source) "PRIVMSG" (channel:message)) = do
    filepath <- logFile channel
    let (n, _, _) = parseUserHost source
    time <- liftIO getZonedTime 
    liftIO $ case filepath of
      Just path -> appendFile path ("[" ++ show time ++ "] <" ++ n ++ "> " ++ unwords message ++ "\n")
      Nothing   -> return ()
logMessage _ = return ()
