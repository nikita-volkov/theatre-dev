module TheatreDev.ExtrasFor.TBQueue where

import Control.Concurrent.STM.TBQueue
import TheatreDev.Prelude

flushNonEmptyTBQueue :: TBQueue a -> STM (NonEmpty a)
flushNonEmptyTBQueue x = do
  head <- readTBQueue x
  tail <- correctFlushTBQueue x
  return (head :| tail)

-- | Get a list of all entries in the queue without removing them.
inspectTBQueue :: TBQueue a -> STM [a]
inspectTBQueue queue = do
  list <- correctFlushTBQueue queue
  forM_ list $ writeTBQueue queue
  return list

-- | Starting from \"stm\" 2.5.2.0 "flushTBQueue" is broken.
-- We're fixing it here.
correctFlushTBQueue :: TBQueue a -> STM [a]
correctFlushTBQueue queue =
  go []
  where
    go !acc = do
      element <- tryReadTBQueue queue
      case element of
        Just element -> go $ element : acc
        Nothing -> return $ reverse acc
