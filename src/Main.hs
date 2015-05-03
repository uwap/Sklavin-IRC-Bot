{-# LANGUAGE TypeOperators #-}
module Main where

import IRC.Types
import IRC.Connection
import IRC.Commands
import IRC.Proto
import IRC.Logging

import Data.Time.Clock

import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted

main :: IO ()
main = start eventListener

eventListener :: RawMessage -> IRC ()
eventListener (RawMessage _ "PING" s)         = pong (head s)
eventListener (RawMessage _ "INVITE" chan)    = mapM_ joinChannel $ tail chan
eventListener msg@(RawMessage _ "PRIVMSG" _)  = onPrivmsg msg
eventListener _                               = return ()

onPrivmsg :: RawMessage -> IRC ()
onPrivmsg (RawMessage (Just source) _ (channel:message)) = do
      let (n, _, _) = parseUserHost source
      case unwords message of
        ('!':xs) -> onCommand n channel $ words xs
        _        -> return ()
onPrivmsg _ = return ()

onCommand :: Nick -> String -> [String] -> IRC ()
onCommand _ _ [] = return ()
onCommand n@(Nick nick) channel commands@(cmd:_args)
    | cmd == "pizza"   = pizza
    | cmd == "time"    = liftIO getCurrentTime >>= privmsg channel . show
    | otherwise        = configuratedCommand n channel commands
  where
    pizza = do
      privmsg channel $ nick ++ ", ich geb dir bescheid, sobald deine Pizza fertig ist."
      handle <- ask
      _ <- liftIO . flip oneShotTimer (mDelay 15) . flip runReaderT handle $
        privmsg channel $ nick ++ ", hey, aufwachen! Schlafmütze! Deine Pizza ist fertig!"
      return ()
