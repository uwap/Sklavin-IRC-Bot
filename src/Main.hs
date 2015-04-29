module Main where

import IRC.Connection
import IRC.Config
import IRC.Commands
import IRC.Proto
import IRC.Logging

import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted

main :: IO ()
main = start eventListener

eventListener :: Message -> IRC ()
eventListener (Message _ (Command "PING") s)         = pong (head s)
eventListener (Message _ (Command "INVITE") chan)    = mapM_ (joinChannel . Channel) $ tail chan
eventListener msg@(Message _ (Command "PRIVMSG") _)  = onPrivmsg msg
eventListener _                                      = return ()

onPrivmsg :: Message -> IRC ()
onPrivmsg (Message (Just (Prefix source)) _ (channel:message)) = do
      let (n, _, _) = parseUserHost source
      case unwords message of
        ('!':xs) -> onCommand n (Channel channel) $ words xs
        _        -> return ()
onPrivmsg _ = return ()

onCommand :: Nick -> Channel -> [String] -> IRC ()
onCommand _ _ [] = return ()
onCommand n@(Nick nick) channel commands@(cmd:_args)
    | cmd == "pizza"   = pizza
    | otherwise        = configuratedCommand n channel commands
  where
    pizza = do
      privmsg channel $ nick ++ ", ich geb dir bescheid, sobald deine Pizza fertig ist."
      handle <- ask
      _ <- liftIO . flip oneShotTimer (mDelay 15) . flip runReaderT handle $
        privmsg channel $ nick ++ ", hey, aufwachen! Schlafmütze! Deine Pizza ist fertig!"
      return ()
