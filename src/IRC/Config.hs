{-# LANGUAGE OverloadedStrings #-}
module IRC.Config where

import IRC.Types

import Data.Text (pack)
import Data.Configurator as C
import Data.Configurator.Types

import Control.Monad.Reader

import System.Directory
import System.FilePath

loadConfig :: IO Config
loadConfig = do
  conf <- load [Required "config.conf"]
  paths <- C.lookup conf "commandPath"
  case paths of
    Nothing   -> return ()
    Just path -> do
      files <- getDirectoryContents path
      let commands = map (path </>) $ filter (liftM2 (&&) (/= ".") (/= "..")) files
      addGroupsToConfig ((,) "Commands." <$> Required <$> commands) conf
  return conf

lookupGlobalConfig :: Configured a => String -> IRC (Maybe a)
lookupGlobalConfig name' = do
  conf <- asks config
  liftIO $ C.lookup conf (pack name')

lookupServerConfig :: Configured a => String -> IRC (Maybe a)
lookupServerConfig name' = do
  server <- asks serverName 
  lookupGlobalConfig (server ++ "." ++ name')
