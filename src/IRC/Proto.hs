module IRC.Proto where

import IRC.Types
import Text.Printf (hPrintf)
import Control.Monad.Reader (asks, liftIO)

{--------------------------------------------------------------}
{-------------------------- Connection ------------------------}
{--------------------------------------------------------------}
write :: String -> IRC ()
write s = do
    h <- asks socket
    liftIO $ hPrintf h "%s\r\n" s

{--------------------------------------------------------------}
{--------------------------- Parsing --------------------------}
{--------------------------------------------------------------}
parseUserHost :: String -> (Nick, Name, Host) -- Nick, Name, Host
parseUserHost s = (nick s, name s, host s)
  where
    nick = takeWhile (/= '!')
    name = takeWhile (/= '@') . drop 1 . dropWhile (/= '!')
    host = drop 1 . dropWhile (/= '@')

{--
 RFC2812 defines a message as
    message    =  [ ":" prefix SPACE ] command [ params ] crlf
 This parses prefix, command and params out of it.
--}
parseCommand :: String -> RawMessage
parseCommand (':':s) = RawMessage (Just $ source s) (cmd s) (parameters s)
                  where
                     source     = takeWhile (/= ' ')
                     cmd        = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
                     parameters = parseParams . dropWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
parseCommand s = RawMessage Nothing (cmd s) (parameters s)
                where
                    cmd        = takeWhile (/= ' ')
                    parameters = parseParams . dropWhile (/= ' ')
{--
 Parsing Params is more complicated then parsing command or source.
 In RFC2812 params are described as follows:
    params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
               =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
               
 The simplest way is to parse middles and trailing seperatively.
 If there are > 14 middles, we join them.
--}
parseParams :: String -> [String]
parseParams s = filter (not . null) $ parseMiddles ++ [parseTrailing s]
            where
              parseMiddles   = let mids = filter (not . null) $ parseMiddles' s in
                        take 14 mids ++ (return . unwords) (drop 14 mids)
              parseMiddles'  = words . takeWhile (/= ':')
              parseTrailing  = drop 1 . dropWhile (/= ':')
              
{--------------------------------------------------------------}
{------------------------ User Commands -----------------------}
{--------------------------------------------------------------}
away :: String -> IRC ()
away awayMessage = write $ "AWAY " ++ awayMessage

back :: IRC ()
back = write "AWAY"

invite :: Nick -> Channel -> IRC ()
invite nick channel = write $ "INVITE " ++ unwords [nick, channel]

quit :: String -> IRC ()
quit quitMessage = write $ "QUIT :" ++ quitMessage

join :: Channel -> IRC ()
join channel = write $ "JOIN " ++ channel

pong :: String -> IRC ()
pong code = write $ "PONG :" ++ code

privmsg :: Channel -> String -> IRC ()
privmsg channel msg = write $ "PRIVMSG " ++ channel ++ " :" ++ msg

act :: Channel -> String -> IRC ()
act channel msg = privmsg channel ('\x0001' : "ACTION " ++ msg ++ "\x0001")
