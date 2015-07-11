module IRC.Channel (
  fromName
) where

import IRC.Types
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M

fromName :: ChannelName -> IRC Channel
fromName name = do
  ref <- asks channelRef
  map' <- liftIO $ readIORef ref
  case M.lookup name map' of
    Just chan -> return chan
    Nothing   -> createChannel name

createChannel :: ChannelName -> IRC Channel
createChannel name = do
  ref <- asks channelRef
  map' <- liftIO $ readIORef ref
  let chan = mkChannel name
  liftIO $ writeIORef ref $ M.insert name chan map'
  return chan

mkChannel :: ChannelName -> Channel
mkChannel = Channel
