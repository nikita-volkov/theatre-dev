{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module TheatreDev.StmBased.StmStructures.Runner
  ( Runner,
    start,
    tell,
    kill,
    wait,
    receiveSingle,
    receiveMultiple,
    releaseWithException,
    releaseNormally,

    -- * Inspection
    getId,
  )
where

import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TMVar
import Data.UUID.V4 qualified as UuidV4
import TheatreDev.ExtrasFor.TBQueue
import TheatreDev.Prelude

data Runner a = Runner
  { queue :: TBQueue a,
    receivesVar :: TVar Bool,
    resVar :: TMVar (Maybe SomeException),
    id :: UUID
  }

getId :: Runner a -> UUID
getId = (.id)

start :: IO (Runner a)
start =
  do
    queue <- newTBQueueIO 1000
    receivesVar <- newTVarIO True
    resVar <- newEmptyTMVarIO
    id <- UuidV4.nextRandom
    return Runner {..}

tell :: Runner a -> a -> STM ()
tell Runner {..} message =
  do
    receives <- readTVar receivesVar
    when receives do
      writeTBQueue queue message

kill :: Runner a -> STM ()
kill Runner {..} =
  writeTVar receivesVar False

wait :: Runner a -> STM (Maybe SomeException)
wait Runner {..} = do
  receives <- readTVar receivesVar
  when receives retry
  queueIsEmpty <- isEmptyTBQueue queue
  unless queueIsEmpty retry
  readTMVar resVar

receiveSingle ::
  Runner a ->
  -- | Action producing a message or nothing, after it's killed.
  STM (Maybe a)
receiveSingle Runner {..} =
  do
    receives <- readTVar receivesVar
    if receives
      then Just <$> readTBQueue queue
      else return Nothing

receiveMultiple ::
  Runner a ->
  STM (Maybe (NonEmpty a))
receiveMultiple Runner {..} =
  do
    messages <- simplerFlushTBQueue queue
    case messages of
      [] -> do
        receives <- readTVar receivesVar
        if receives
          then retry
          else return Nothing
      messagesHead : messagesTail ->
        return $ Just $ messagesHead :| messagesTail

releaseWithException :: Runner a -> SomeException -> STM ()
releaseWithException Runner {..} exception =
  do
    simplerFlushTBQueue queue
    putTMVar resVar (Just exception)

releaseNormally :: Runner a -> STM ()
releaseNormally Runner {..} =
  putTMVar resVar Nothing <|> pure ()
