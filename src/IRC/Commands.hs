{-# LANGUAGE OverloadedStrings #-}
module IRC.Commands where

import IRC.Types
import IRC.Config
import IRC.Proto

import System.Random (randomIO)

import Control.Monad
import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted

import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Utils (replace)
import Data.Time.Clock
import Data.List (isSuffixOf)
import Data.List.Split
import Data.Char

import Text.Read

configuratedCommand :: User -> Channel -> [String] -> IRC ()
configuratedCommand _ _ [] = return ()
configuratedCommand nick channel (comm:args) = do
    commands <- fromMaybe [] <$> lookupGlobalConfig ("Commands." ++ (toLower <$> comm) ++ ".reply")
    unless (null commands) $ do
      random <- liftIO randomIO
      let cmd = commands !! (random `mod` length commands)
      forM_ (lines cmd) evaluateLine 
  where
    evaluateLine :: String -> IRC ()
    evaluateLine rawline = do
      line <- replaceVars rawline
      case listToMaybe (words rawline) of
        Nothing -> return ()
        Just "/me"     -> act channel $ drop 4 line
        Just "/delay"  -> executeDelay $ drop 7 rawline
        Just "/choose" -> choose $ drop 8 line
        Just _         -> privmsg channel line

    executeDelay :: String -> IRC ()
    executeDelay line = case words line of
             (time:reply) -> do
               realtime <- replaceVars time
               case asSeconds realtime :: Maybe Double of
                 Nothing      -> privmsg channel $ time ++ " is not a valid time"
                 Just seconds -> delayReply (sDelay $ round seconds) $ evaluateLine (unwords reply)
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

    choose :: String -> IRC ()
    choose str = do
        let list = trim <$> splitOneOf ",;" str
        privmsg channel =<< pickRandom list
      where
        trim :: String -> String
        trim = dropWhile (==' ') . reverse . dropWhile (==' ') . reverse

pickRandom :: [String] -> IRC String
pickRandom list = liftIO randomIO >>= \r -> return $ list !! (r `mod` length list)

asSeconds :: (Num a, Read a) => String -> Maybe a
asSeconds str | "s" `isSuffixOf` str = readMaybe (init str)
asSeconds str | "m" `isSuffixOf` str = (*60)   <$> readMaybe (init str)
asSeconds str | "h" `isSuffixOf` str = (*3600) <$> readMaybe (init str)
asSeconds str                        = readMaybe str
