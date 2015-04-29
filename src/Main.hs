module Main where

import IRC.Connection
import IRC.Config
import IRC.Commands
import IRC.Proto

import System.Random (randomIO)
import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted
import Data.Text (pack)

main :: IO ()
main = start eventListener

eventListener :: Message -> IRC ()
eventListener msg@(Message _ (Command "PING") s)         = pong (head s)
eventListener msg@(Message _ (Command "INVITE") chan)    = mapM_ (joinChannel . Channel) $ tail chan
eventListener msg@(Message p (Command "PRIVMSG") params) = onPrivmsg msg
eventListener msg@(Message {})                           = return ()

onPrivmsg :: Message -> IRC ()
onPrivmsg (Message (Just (Prefix source)) _ (channel:message)) = do
      let (n, _, _) = parseUserHost source
      case unwords message of
        ('!':xs) -> onCommand n (Channel channel) $ words xs
        _        -> return ()
onPrivmsg _ = return ()

onCommand :: Nick -> Channel -> [String] -> IRC ()
onCommand nnick@(Nick nick) channel@(Channel chan) clist@(command:args)
    | command == "penis"   = liftIO randomIO >>= \r -> privmsg channel $ "8" ++ replicate (r `mod` 30) '=' ++ "D"
    | command == "pizza"   = pizza channel nick
    | otherwise            = configuratedCommand nnick channel clist $ pack ("Commands." ++ command)
  where
    pizza chan nick = do
      privmsg chan $ nick ++ ", ich geb dir bescheid, sobald deine Pizza fertig ist."
      handle <- ask
      liftIO . flip oneShotTimer (mDelay 15) . flip runReaderT handle $
        privmsg chan $ nick ++ ", hey, aufwachen! Schlafmütze! Deine Pizza ist fertig!"
      return ()
