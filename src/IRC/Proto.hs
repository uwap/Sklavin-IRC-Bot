module IRC.Proto where

import IRC.Types
import IRC.Channel
import Text.Printf (hPrintf)
import Control.Lens
import Control.Monad.State

{--------------------------------------------------------------}
{-------------------------- Connection ------------------------}
{--------------------------------------------------------------}
write :: String -> IRC ()
write s = do
    h <- use socket
    liftIO $ hPrintf h "%s\r\n" s

{--------------------------------------------------------------}
{--------------------------- Parsing --------------------------}
{--------------------------------------------------------------}
parseUserHost :: String -> (User, Name, Host) -- User, Name, Host
parseUserHost s = (nick s, name' s, host s)
  where
    nick  = takeWhile (/= '!')
    name' = takeWhile (/= '@') . drop 1 . dropWhile (/= '!')
    host  = drop 1 . dropWhile (/= '@')

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
{--------------------------- Message --------------------------}
{--------------------------------------------------------------}
fromRawMessage :: RawMessage -> IRC Message
fromRawMessage msg = do
  let (user, _, _) = case prefix msg of {
      Just s  -> parseUserHost s;
      Nothing -> ("UNKNOWN", "UNKNOWN", "UNKNOWN") }
  case msg of
    (RawMessage _ "PRIVMSG" (channel:message)) -> return . Privmsg user (unwords message) =<< fromName channel
    (RawMessage _ "PING" code)                 -> return $ Ping (unwords code)
    (RawMessage _ "INVITE" (nick:channel))     -> return . Invite nick =<< fromName (unwords channel)
    (RawMessage _ "QUIT" message)              -> return $ Quit user (unwords message)
    (RawMessage _ "PART" (channel:message))    -> return . Part user (unwords message) =<< fromName channel
    (RawMessage _ "JOIN" channel)              -> return . Join user =<< fromName (unwords channel)
    _                                          -> return $ Raw msg

{--------------------------------------------------------------}
{------------------------ User Commands -----------------------}
{--------------------------------------------------------------}
away :: String -> IRC ()
away awayMessage = write $ "AWAY " ++ awayMessage

back :: IRC ()
back = write "AWAY"

invite :: User -> Channel -> IRC ()
invite nick channel = write $ "INVITE " ++ unwords [nick, channel ^. name]

quit :: String -> IRC ()
quit quitMessage = write $ "QUIT :" ++ quitMessage

joinChannel :: Channel -> IRC ()
joinChannel channel = write $ "JOIN " ++ (channel ^. name)

pong :: String -> IRC ()
pong code = write $ "PONG :" ++ code

privmsg :: Channel -> String -> IRC ()
privmsg channel msg = forM_ (lines msg) privmsgLine
  where
    privmsgLine m = unless (null m) $ write $ "PRIVMSG " ++ (channel ^. name) ++ " :" ++ m

act :: Channel -> String -> IRC ()
act channel msg = forM_ (lines msg) actLine
  where
    actLine m = unless (null m) $ privmsg channel ('\x0001' : "ACTION " ++ m ++ "\x0001")
