{-# LANGUAGE OverloadedStrings #-}
module IRC.Commands where

import IRC.Connection
import IRC.Proto
import IRC.Config

import System.Random (randomIO)

import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader

import Data.Text (pack, unpack, replace)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

away :: String -> IRC ()
away = write . ucAway . Just

back :: IRC ()
back = write $ ucAway Nothing

invite :: Nick -> Channel -> IRC ()
invite n = write . ucInvite n

joinChannel :: Channel -> IRC ()
joinChannel c = write $ ucJoin c

pong :: String -> IRC ()
pong = write . ucPong

privmsg :: Channel -> String -> IRC ()
privmsg c = write . ucPrivmsg c

configuratedCommand :: Nick -> Channel -> [String] -> C.Name -> IRC ()
configuratedCommand (Nick nick) chan@(Channel channel) (comm:args) name = do
  conf <- asks config
  maybeCommand <- liftIO $ C.lookup conf name
  case maybeCommand of
    Nothing -> return ()
    Just commands -> do
      command <- liftIO randomIO >>= \r -> return $ commands !! (r `mod` length commands)
      let n = replace "@nick" (pack nick) $ pack command
      let c = replace "@channel" (pack channel) n
      let a = replace "@args" (pack $ unwords args) c
      privmsg chan $ unpack a
