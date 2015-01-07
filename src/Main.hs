module Main where

import IRC.Connection
import IRC.Config
import IRC.Commands
import IRC.Proto

import System.Random (randomIO)
import Control.Monad.Reader (liftIO)

main :: IO ()
main = start $ Config "irc.german-elite.net" 6667 "Sklavin" eventListener

eventListener :: Message -> IRC ()
eventListener (Message _ (Command "PING") s)         = pong (head s)
eventListener (Message _ (Command "INVITE") chan)    = mapM_ (joinChannel . Channel) $ tail chan
eventListener (Message p (Command "PRIVMSG") params) = onPrivmsg $ Message p (Command "PRIVMSG") params
eventListener (Message _ _ _)                        = return ()

onPrivmsg :: Message -> IRC ()
onPrivmsg (Message p _ params) =
  case p of
    Nothing              -> return ()
    Just (Prefix source) -> do
      let (n, _, _) = parseUserHost source
      case last params of
        ('!':xs) -> onCommand n (Channel $ head params) $ words xs
        _        -> return ()

onCommand :: Nick -> Channel -> [String] -> IRC ()
onCommand (Nick nick) (Channel chan) (command:args)
  | command == "hi"      = privmsg (Channel chan) $ "Hey " ++ nick
  | command == "penis"   = privmsg (Channel chan) $ "8=========D"
  | command == "meinung" = meinung args >>= privmsg (Channel chan)
  | otherwise            = return ()

meinung :: [String] -> IRC String
meinung args = do
  let arg = unwords args
  random <- liftIO randomIO
  return $ arg ++ " " ++ (meinungen !! (random `mod` length meinungen))
  
meinungen :: [String]
meinungen = [ "finde ich voll toll"
            , "ist doch mega scheiße"
            , "interessiert bitte wen?!"
            ]
