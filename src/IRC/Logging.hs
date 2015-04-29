module IRC.Logging where

import IRC.Config
import IRC.Proto

import System.Directory
import System.FilePath.Posix

import Control.Monad
import Control.Monad.Reader

logFile :: Channel -> IRC (Maybe FilePath)
logFile (Channel chan) = do
    serverLogPath <- lookupServerConfig "logs"
    let filePath = serverLogPath >>= \path -> return (path </> chan <.> "log")
    case filePath of
      Just path -> liftIO $ liftM (fromBool path) $ doesFileExist path
      Nothing -> return Nothing
  where
    fromBool a True  = Just a
    fromBool _ False = Nothing
