module Core.IRC.Util where

import Core.IRC.Types
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Control (liftBaseDiscard)

forkIRC :: IRC () -> IRC ThreadId
forkIRC = liftBaseDiscard forkIO

forkIRC_ :: IRC () -> IRC ()
forkIRC_ = void . forkIRC
