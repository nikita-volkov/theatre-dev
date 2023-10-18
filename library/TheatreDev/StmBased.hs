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

import TheatreDev.Prelude
import TheatreDev.StmBased.StmStructures.Runner (Runner)
import qualified TheatreDev.StmBased.StmStructures.Runner as Runner
import TheatreDev.StmBased.Tell (Tell)
import qualified TheatreDev.StmBased.Tell as Tell
import qualified TheatreDev.StmBased.Wait as Wait

-- |
-- Controls of an actor, which processes the messages of type @message@.
--
-- Provides abstraction over the message channel, thread-forking and killing.
--
-- Monoid instance is not provided for the same reason it is not provided for numbers.
-- This type supports both sum and product composition. See 'allOf' and 'oneOf'.
data Actor message = Actor
  { -- | Send a message to the actor.
    tell :: message -> STM (),
    -- | Kill the actor.
    kill :: STM (),
    -- | Wait for the actor to die due to error or being killed.
    wait :: STM (Maybe SomeException)
  }

instance Contravariant Actor where
  contramap fn (Actor tell kill wait) =
    Actor (tell . fn) kill wait

instance Divisible Actor where
  conquer =
    Actor (const (return ())) (return ()) (return Nothing)
  divide divisor (Actor lTell lKill lWait) (Actor rTell rKill rWait) =
    Actor tell kill wait
    where
      tell msg = case divisor msg of (lMsg, rMsg) -> lTell lMsg >> rTell rMsg
      kill = lKill >> rKill
      wait = Wait.both lWait rWait

instance Decidable Actor where
  lose fn =
    Actor (const (return ()) . absurd . fn) (return ()) (return Nothing)
  choose choice (Actor lTell lKill lWait) (Actor rTell rKill rWait) =
    Actor tell kill wait
    where
      tell = either lTell rTell . choice
      kill = lKill >> rKill
      wait = Wait.both lWait rWait

-- * Composition

fromRunner :: Runner a -> Actor a
fromRunner runner =
  Actor
    { tell = Runner.tell runner,
      kill = Runner.kill runner,
      wait = Runner.wait runner
    }

-- | Distribute the message stream across actors.
-- The message gets delivered to the first available one.
--
-- E.g., using this combinator in combination with 'replicateM'
-- you can construct pools:
--
-- > spawnPool :: Int -> IO (Actor message) -> IO (Actor message)
-- > spawnPool size spawn =
-- >   oneOf <$> replicateM size spawn
--
-- You can consider this being an interface to the Sum monoid.
oneOf :: [Actor message] -> Actor message
oneOf = composition Tell.one

-- |
--
-- You can consider this being an interface to the Product monoid.
allOf :: [Actor message] -> Actor message
allOf = composition Tell.all

composition :: ([Tell message] -> Tell message) -> [Actor message] -> Actor message
composition tellReducer actors =
  Actor
    { tell = tellReducer (fmap (.tell) actors),
      kill = traverse_ (.kill) actors,
      wait = Wait.all (fmap (.wait) actors)
    }

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
spawnStatelessIndividual interpreter cleaner =
  -- TODO: Optimize by reimplementing directly.
  spawnStatefulIndividual () (const interpreter) (const cleaner)

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
    runner <- atomically Runner.start
    forkIOWithUnmask $ \unmask ->
      let loop !state =
            do
              message <- atomically $ Runner.receiveSingle runner
              case message of
                Just message ->
                  do
                    result <- try @SomeException $ unmask $ step state message
                    case result of
                      Right newState ->
                        loop newState
                      Left exception ->
                        do
                          atomically $ Runner.releaseWithException runner exception
                          finalizer state
                Nothing ->
                  finalizer state
       in loop zero
    return $ fromRunner runner

spawnStatefulBatched ::
  state ->
  (state -> NonEmpty message -> IO state) ->
  (state -> IO ()) ->
  IO (Actor message)
spawnStatefulBatched zero step finalizer =
  do
    runner <- atomically Runner.start
    forkIOWithUnmask $ \unmask ->
      let loop !state =
            do
              messages <- atomically $ Runner.receiveMultiple runner
              case nonEmpty messages of
                Just nonEmptyMessages ->
                  do
                    result <- try @SomeException $ unmask $ step state nonEmptyMessages
                    case result of
                      Right newState ->
                        loop newState
                      Left exception ->
                        do
                          atomically $ Runner.releaseWithException runner exception
                          finalizer state
                -- Empty batch means that the runner is finished.
                Nothing -> finalizer state
       in loop zero
    return $ fromRunner runner

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
