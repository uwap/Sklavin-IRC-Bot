module IRC.Config where

data Config = Config { server :: String
                     , port   :: Int
                     , nick   :: String
                     }
