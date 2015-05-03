module IRC.Config where

import IRC.Types
import IRC.Proto

import Data.Text
import Data.Configurator
import Data.Configurator.Types

import Control.Monad.Reader

loadConfig :: IO Config
loadConfig = load [Required "config.conf"]

lookupGlobalConfig :: Configured a => String -> IRC (Maybe a)
lookupGlobalConfig name = do
  conf <- asks config
  liftIO $ Data.Configurator.lookup conf (pack name)

lookupServerConfig :: Configured a => String -> IRC (Maybe a)
lookupServerConfig name = do
  server <- asks serverName 
  lookupGlobalConfig (server ++ "." ++ name)
