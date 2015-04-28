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

main :: IO ()
main = start eventListener-- $ Config "irc.hackint.net" 6667 "Sklavin" eventListener

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
onCommand (Nick nick) channel@(Channel chan) (command:args)
    | command == "hi"      = privmsg channel $ "Hey " ++ nick
    | command == "penis"   = liftIO randomIO >>= \r -> privmsg channel $ "8" ++ replicate (r `mod` 30) '=' ++ "D"
    | command == "meinung" = meinung args >>= privmsg channel
    | command == "pizza"   = pizza channel nick
    | otherwise            = return ()
  where
    pizza chan nick = do
      privmsg chan $ nick ++ ", ich geb dir bescheid, sobald deine Pizza fertig ist."
      handle <- ask
      liftIO . flip oneShotTimer (mDelay 15) . flip runReaderT handle $
        privmsg chan $ nick ++ ", hey, aufwachen! Schlafmütze! Deine Pizza ist fertig!"
      return ()

meinung :: [String] -> IRC String
meinung args = do
  let arg = unwords args
  random <- liftIO randomIO
  return $ arg ++ " " ++ (meinungen !! (random `mod` length meinungen))
  
meinungen :: [String]
meinungen = [ "finde ich voll toll"
            , "ist doch mega scheiße"
            , "interessiert bitte wen?!"
            , "knuddel ich voll gern"
            , "geht mir am Arsch vorbei"
            , "ist mein Superheld"
            , "hat mir Kekse versprochen, aber ich habe nie welche bekommen =("
            , "kann mich mal in den Arsch ficken"
            , "… Untenrum!"
            , "ist fett"
            , "geht voll ab"
            , "finde ich nicht so mega"
            , "ist dumm wie Stroh"
            , "got owned"
            , "mag keine Pizza!"
            , "ist nicht so toll wie uwap"
            ]
