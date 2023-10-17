module TheatreDev.ExtrasFor.TBQueue where

import Control.Concurrent.STM.TBQueue
import TheatreDev.Prelude

flushNonEmptyTBQueue :: TBQueue a -> STM (NonEmpty a)
flushNonEmptyTBQueue x = do
  head <- readTBQueue x
  tail <- flushTBQueue x
  return (head :| tail)
