module IRC.Commands where

import IRC.Connection
import IRC.Proto

away :: String -> IRC ()
away = write . ucAway . Just

back :: IRC ()
back = write $ ucAway Nothing

invite :: Nick -> Channel -> IRC ()
invite n = write . ucInvite n
