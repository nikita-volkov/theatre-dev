{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module TheatreDev.StmBased
  ( Actor,

    -- * Acquisition
    spawnStatefulIndividual,
    spawnStatefulBatched,
    spawnStatelessIndividual,
    spawnStatelessBatched,

    -- * Control
    tell,
    kill,
    wait,

    -- * Composition
    oneOf,
    allOf,
  )
where

import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TMVar
import qualified TheatreDev.ExtrasFor.List as List
import TheatreDev.ExtrasFor.TBQueue
import TheatreDev.Prelude

-- |
-- Controls of an actor, which processes the messages of type @message@.
--
-- Provides abstraction over the message channel, thread-forking and killing.
data Actor message = Actor
  { -- | Send a message to the actor.
    tell :: message -> STM (),
    -- | Kill the actor.
    kill :: STM (),
    -- | Wait for the actor to die due to error or being killed.
    wait :: STM (Maybe SomeException)
  }

instance Semigroup (Actor message) where
  (<>) (Actor lTell lKill lWait) (Actor rTell rKill rWait) =
    Actor tell kill wait
    where
      tell msg = lTell msg >> rTell msg
      kill = lKill >> rKill
      wait = (<|>) <$> lWait <*> rWait

instance Monoid (Actor message) where
  mempty =
    Actor (const (return ())) (return ()) (return Nothing)

instance Contravariant Actor where
  contramap fn (Actor tell kill wait) =
    Actor (tell . fn) kill wait

instance Divisible Actor where
  conquer =
    mempty
  divide divisor (Actor lTell lKill lWait) (Actor rTell rKill rWait) =
    Actor tell kill wait
    where
      tell msg = case divisor msg of (lMsg, rMsg) -> lTell lMsg >> rTell rMsg
      kill = lKill >> rKill
      wait = (<|>) <$> lWait <*> rWait

instance Decidable Actor where
  lose fn =
    Actor (const (return ()) . absurd . fn) (return ()) (return Nothing)
  choose choice (Actor lTell lKill lWait) (Actor rTell rKill rWait) =
    Actor tell kill wait
    where
      tell = either lTell rTell . choice
      kill = lKill >> rKill
      wait = (<|>) <$> lWait <*> rWait

-- * Acquisition

-- |
-- Given an interpreter of messages,
-- fork a thread to run the handler daemon on and
-- produce a handle to control that actor.
--
-- Killing that actor will make it process all the messages in the queue first.
-- All the messages sent to it after killing won't be processed.
spawnStatelessIndividual ::
  -- | Interpreter of a message.
  (message -> IO ()) ->
  -- | Clean up when killed.
  IO () ->
  -- | Fork a thread to run the handler daemon on and
  -- produce a handle to control it.
  IO (Actor message)
spawnStatelessIndividual handler cleanUp =
  do
    queue <- newTBQueueIO 1000
    aliveVar <- newTVarIO True
    resVar <- newEmptyTMVarIO @(Maybe SomeException)
    forkIOWithUnmask $ \unmask ->
      let loop =
            join $ atomically $ do
              message <- readTBQueue queue
              case message of
                Just message -> return $ do
                  result <- try @SomeException $ unmask $ handler message
                  case result of
                    Right () ->
                      loop
                    Left exception -> do
                      atomically $ do
                        flushTBQueue queue
                        writeTVar aliveVar False
                        putTMVar resVar (Just exception)
                      cleanUp
                Nothing -> do
                  flushTBQueue queue
                  writeTVar aliveVar False
                  putTMVar resVar Nothing
                  return $ cleanUp
       in loop
    return
      Actor
        { tell = \message -> do
            alive <- readTVar aliveVar
            when alive
              $ writeTBQueue queue
              $ Just message,
          kill = do
            alive <- readTVar aliveVar
            when alive
              $ writeTBQueue queue Nothing,
          wait = readTMVar resVar
        }

spawnStatelessBatched ::
  -- | Interpreter of a batch of messages.
  (NonEmpty message -> IO ()) ->
  -- | Clean up when killed.
  IO () ->
  -- | Fork a thread to run the handler daemon on and
  -- produce a handle to control it.
  IO (Actor message)
spawnStatelessBatched interpreter cleaner =
  -- TODO: Optimize by reimplementing directly.
  spawnStatefulBatched () (const interpreter) (const cleaner)

spawnStatefulIndividual ::
  state ->
  (state -> message -> IO state) ->
  (state -> IO ()) ->
  IO (Actor message)
spawnStatefulIndividual zero step finalizer =
  do
    queue <- newTBQueueIO 1000
    aliveVar <- newTVarIO True
    resVar <- newEmptyTMVarIO @(Maybe SomeException)
    forkIOWithUnmask $ \unmask ->
      let loop !state =
            join $ atomically $ do
              message <- readTBQueue queue
              case message of
                Just message -> return $ do
                  result <- try @SomeException $ unmask $ step state message
                  case result of
                    Right newState ->
                      loop newState
                    Left exception -> do
                      atomically $ do
                        flushTBQueue queue
                        writeTVar aliveVar False
                        putTMVar resVar (Just exception)
                      finalizer state
                Nothing -> do
                  flushTBQueue queue
                  writeTVar aliveVar False
                  putTMVar resVar Nothing
                  return $ finalizer state
       in loop zero
    return
      Actor
        { tell = \message -> do
            alive <- readTVar aliveVar
            when alive
              $ writeTBQueue queue
              $ Just message,
          kill = do
            alive <- readTVar aliveVar
            when alive
              $ writeTBQueue queue Nothing,
          wait = readTMVar resVar
        }

spawnStatefulBatched :: state -> (state -> NonEmpty message -> IO state) -> (state -> IO ()) -> IO (Actor message)
spawnStatefulBatched zero step finalizer =
  do
    queue <- newTBQueueIO 1000
    aliveVar <- newTVarIO True
    resVar <- newEmptyTMVarIO @(Maybe SomeException)
    forkIOWithUnmask $ \unmask ->
      let loop !state =
            join $ atomically $ do
              flushing <- flushNonEmptyTBQueue queue
              let (messages, flushingTail) = List.splitWhileJust (toList flushing)
              case messages of
                -- Implies that the tail is not empty.
                -- And that it starts with a Nothing.
                [] -> do
                  writeTVar aliveVar False
                  putTMVar resVar Nothing
                  return $ do
                    finalizer state
                messagesHead : messagesTail ->
                  return $ do
                    result <-
                      try @SomeException
                        $ unmask
                        $ step state (messagesHead :| messagesTail)
                    case result of
                      Right newState ->
                        case flushingTail of
                          [] -> loop newState
                          _ -> do
                            atomically $ do
                              writeTVar aliveVar False
                              putTMVar resVar Nothing
                            finalizer state
                      Left exception -> do
                        atomically $ do
                          writeTVar aliveVar False
                          putTMVar resVar Nothing
                        finalizer state
       in loop zero
    return
      Actor
        { tell = \message -> do
            alive <- readTVar aliveVar
            when alive
              $ writeTBQueue queue
              $ Just message,
          kill = do
            alive <- readTVar aliveVar
            when alive
              $ writeTBQueue queue Nothing,
          wait = readTMVar resVar
        }

-- * Control

tell :: Actor message -> message -> IO ()
tell actor =
  atomically . actor.tell

kill :: Actor message -> IO ()
kill actor =
  atomically actor.kill

wait :: Actor message -> IO ()
wait actor =
  atomically actor.wait >>= maybe (pure ()) throwIO

-- * Composition

-- | Distribute the message across the available actors.
-- The message will be delivered to the first available actor.
--
-- Using this combinator in combination with 'replicateM' and some spawner,
-- you can construct pools.
oneOf :: [Actor message] -> Actor message
oneOf actors =
  Actor {tell, kill, wait}
  where
    tell msg =
      asum (fmap (($ msg) . (.tell)) actors)
    kill =
      traverse_ (.kill) actors
    wait =
      fmap asum (traverse (.wait) actors)

allOf :: [Actor message] -> Actor message
allOf actors =
  Actor {tell, kill, wait}
  where
    tell msg =
      forM_ actors $ \actor -> actor.tell msg
    kill =
      traverse_ (.kill) actors
    wait =
      fmap asum (traverse (.wait) actors)
