module IRC.Proto where

import Data.Foldable (toList)

{--------------------------------------------------------------}
{--------------------------- Parsing --------------------------}
{--------------------------------------------------------------}

parseUserHost :: String -> (String, String, String) -- Nick, Name, Host
parseUserHost s = (nick s, name s, host s)
  where
    nick = takeWhile (/= '!')
    name = takeWhile (/= '@') . drop 1 . dropWhile (/= '!')
    host = drop 1 . dropWhile (/= '@')

data RawMessage = RawMessage { prefix  :: Maybe String
                             , command :: String
                             , params  :: [String]
                             }

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
 Therefor we add an extra parsing function.
 There are various cases to handle:
 1. There aren't any parameters
 2. There are up to 14 parameters
 3. There are up to 14 parameters and then a trailing parameter
 4. There is just a trailing parameter
 5. There are 14 parameters and then a trailing paramter
 6. There are 14 parameters and a trailing parameter without a colon
 
 In RFC2812 params are described as follows:
    params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
               =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
               
 The simplest way is to parse middles and trailing seperatively.
 If there are > 14 middles, we join them.
--}
parseParams :: String -> [String]
parseParams s = filter (not . null) $ parseMiddles ++ [parseTrailing s]
            where
              parseMiddles   = let middles = filter (not . null) $ parseMiddles' s in
                        take 14 middles ++ (return . unwords) (drop 14 middles)
              parseMiddles'  = words . takeWhile (/= ':')
              parseTrailing  = drop 1 . dropWhile (/= ':')
              
{--------------------------------------------------------------}
{------------------------ User Commands -----------------------}
{--------------------------------------------------------------}

uc :: String -> [String] -> Maybe String -> String
uc command middles Nothing = command ++ " " ++ unwords middles
uc command middles (Just trailing) = command ++ " " ++ unwords middles ++ " :" ++ trailing

ucAway :: Maybe String -> String
ucAway message = uc "AWAY" (toList message) Nothing

ucInvite :: String -> String -> String
ucInvite nick channel = uc "INVITE" [nick, channel] Nothing

ucQuit :: Maybe String -> String
ucQuit = uc "QUIT" []

ucJoin :: String -> String
ucJoin channel = uc "JOIN" (return channel) Nothing

ucPong :: String -> String
ucPong code = uc "PONG" [] $ Just code

ucPrivmsg :: String -> String -> String
ucPrivmsg channel msg = uc "PRIVMSG" (return channel) $ Just msg
