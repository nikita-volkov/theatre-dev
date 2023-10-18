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
  )
where

import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TMVar
import qualified TheatreDev.ExtrasFor.List as List
import TheatreDev.Prelude

data Runner a = Runner
  { queue :: TBQueue (Maybe a),
    aliveVar :: TVar Bool,
    resVar :: TMVar (Maybe SomeException)
  }

start :: STM (Runner a)
start =
  do
    queue <- newTBQueue 1000
    aliveVar <- newTVar True
    resVar <- newEmptyTMVar @(Maybe SomeException)
    return Runner {..}

tell :: Runner a -> a -> STM ()
tell Runner {..} message =
  do
    alive <- readTVar aliveVar
    when alive
      $ writeTBQueue queue
      $ Just message

kill :: Runner a -> STM ()
kill Runner {..} =
  do
    alive <- readTVar aliveVar
    when alive
      $ writeTBQueue queue Nothing

wait :: Runner a -> STM (Maybe SomeException)
wait Runner {..} =
  readTMVar resVar

receiveSingle ::
  Runner a ->
  -- | Action producing a message or nothing, after it's killed.
  STM (Maybe a)
receiveSingle Runner {..} =
  do
    message <- readTBQueue queue
    case message of
      Just message -> return (Just message)
      Nothing -> do
        flushTBQueue queue
        writeTVar aliveVar False
        putTMVar resVar Nothing
        return Nothing

receiveMultiple :: Runner a -> STM [a]
receiveMultiple Runner {..} =
  do
    messages <- do
      head <- readTBQueue queue
      tail <- flushTBQueue queue
      return $ fst $ List.splitWhileJust $ head : tail
    case messages of
      -- Implies that the tail is not empty.
      -- And that it starts with a Nothing.
      [] -> do
        writeTVar aliveVar False
        putTMVar resVar Nothing
        return []
      _ -> do
        unGetTBQueue queue Nothing
        return messages

releaseWithException :: Runner a -> SomeException -> STM ()
releaseWithException Runner {..} exception =
  do
    flushTBQueue queue
    writeTVar aliveVar False
    putTMVar resVar (Just exception)