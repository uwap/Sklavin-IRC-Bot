{-# LANGUAGE OverloadedStrings #-}
module IRC.Commands where

import IRC.Types
import IRC.Connection
import IRC.Config
import qualified IRC.Proto as P

import System.Random (randomIO)

import Control.Monad
import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted

import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Utils (replace)
import Data.Time.Clock
import Data.List (isSuffixOf)

import Text.Read

away :: String -> IRC ()
away = write . P.evaluateUserCommand . P.away . Just

back :: IRC ()
back = write . P.evaluateUserCommand $ P.away Nothing

invite :: String -> String -> IRC ()
invite nick chan = write . P.evaluateUserCommand $ P.invite nick chan

joinChannel :: String -> IRC ()
joinChannel chan = write . P.evaluateUserCommand $ P.join chan

pong :: String -> IRC ()
pong = write . P.evaluateUserCommand . P.pong

privmsg :: String -> String -> IRC ()
privmsg chan = write . P.evaluateUserCommand . P.privmsg chan

act :: String -> String -> IRC ()
act chan = write . P.evaluateUserCommand . P.act chan
-------------------------------------------------------------
configuratedCommand :: String -> String -> [String] -> IRC ()
configuratedCommand _ _ [] = return ()
configuratedCommand nick channel (comm:args) = do
    commands <- fromMaybe [] <$> lookupGlobalConfig ("Commands." ++ comm ++ ".reply")
    unless (null commands) $ do
      random <- liftIO randomIO
      let command = commands !! (random `mod` length commands)
      forM_ (lines command) $ \rawline -> do
        line <- replaceVars rawline
        case listToMaybe (words rawline) of
          Nothing -> return ()
          Just "/me"    -> act channel $ drop 4 line
          Just "/delay" -> executeDelay line
          Just _        -> privmsg channel line
  where
    executeDelay :: String -> IRC ()
    executeDelay line = case drop 1 $ words line of
             (time:reply) -> case asSeconds time of
                               Nothing -> privmsg channel $ time ++ " is not a valid time"
                               Just seconds -> delayReply (sDelay seconds) $ privmsg channel (unwords reply)
             _ -> return ()

    replaceVars :: String -> IRC String
    replaceVars line = do
      let replaceAll = foldl (flip (uncurry replace))
      argsr <- replaceArgs
      time <- liftIO getCurrentTime 
      return $ replaceAll line [("@nick@",nick), ("@channel@",channel), ("@time@",show time), ("@args@",argsr)]
  
    replaceArgs :: IRC String
    replaceArgs = do
      setting <- return . fromMaybe False =<< lookupGlobalConfig ("Commands." ++ comm ++ ".replaceEmptyArgsWithNick")
      case (setting, null args) of
        (True, True) -> return nick
        _            -> return (unwords args)

    delayReply :: Delay -> IRC () -> IRC ()
    delayReply delay reply = do
      handle <- ask
      _ <- liftIO $ flip oneShotTimer delay $ runReaderT reply handle
      return ()

asSeconds :: (Num a, Read a) => String -> Maybe a
asSeconds str | "s" `isSuffixOf` str = readMaybe (init str)
asSeconds str | "m" `isSuffixOf` str = (*60)   <$> readMaybe (init str)
asSeconds str | "h" `isSuffixOf` str = (*3600) <$> readMaybe (init str)
asSeconds str                        = readMaybe str
