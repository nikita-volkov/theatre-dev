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
    byKeyHash,
  )
where

import Data.UUID.V4 qualified as UuidV4
import TheatreDev.Prelude
import TheatreDev.StmBased.StmStructures.Runner (Runner)
import TheatreDev.StmBased.StmStructures.Runner qualified as Runner
import TheatreDev.StmBased.Tell (Tell)
import TheatreDev.StmBased.Tell qualified as Tell
import TheatreDev.StmBased.Wait qualified as Wait

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
    wait :: STM (Maybe SomeException),
    -- | IDs of the constituent actors.
    -- Useful for debugging.
    ids :: [UUID]
  }

instance Contravariant Actor where
  contramap fn (Actor tell kill wait ids) =
    Actor (tell . fn) kill wait ids

instance Divisible Actor where
  conquer =
    Actor (const (return ())) (return ()) (return Nothing) []
  divide divisor (Actor lTell lKill lWait lIds) (Actor rTell rKill rWait rIds) =
    Actor
      { tell = \msg -> case divisor msg of (lMsg, rMsg) -> lTell lMsg >> rTell rMsg,
        kill = lKill >> rKill,
        wait = Wait.both lWait rWait,
        ids = lIds <> rIds
      }

instance Decidable Actor where
  lose fn =
    Actor (const (return ()) . absurd . fn) (return ()) (return Nothing) []
  choose choice (Actor lTell lKill lWait lIds) (Actor rTell rKill rWait rIds) =
    Actor
      { tell = either lTell rTell . choice,
        kill = lKill >> rKill,
        wait = Wait.both lWait rWait,
        ids = lIds <> rIds
      }

-- * Composition

fromIdentifiedRunner :: UUID -> Runner a -> Actor a
fromIdentifiedRunner id runner =
  Actor
    { tell = Runner.tell runner,
      kill = Runner.kill runner,
      wait = Runner.wait runner,
      ids = [id]
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
oneOf = tellComposition Tell.one

-- |
--
-- You can consider this being an interface to the Product monoid.
allOf :: [Actor message] -> Actor message
allOf = tellComposition Tell.all

-- |
-- Dispatch the message across actors based on a key hash.
--
-- This lets you ensure of a property that messages with
-- the same key will arrive to the same actor,
-- letting you maintain a local associated state in the actors.
--
-- The implementation applies a modulo equal to the amount
-- of actors to the hash and thus determines the index
-- of the actor to dispatch the message to.
-- This is inspired by how partitioning is done in Kafka.
byKeyHash ::
  -- | Function extracting the key from the message and hashing it.
  (message -> Int) ->
  -- | Pool of actors.
  [Actor message] ->
  Actor message
byKeyHash = tellComposition . Tell.byKeyHash

tellComposition :: ([Tell message] -> Tell message) -> [Actor message] -> Actor message
tellComposition tellReducer actors =
  Actor
    { tell = tellReducer (fmap (.tell) actors),
      kill = traverse_ (.kill) actors,
      wait = Wait.all (fmap (.wait) actors),
      ids = foldMap (.ids) actors
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
  (Show message) =>
  -- | Clean up when killed.
  IO () ->
  -- | Interpreter of a message.
  (message -> IO ()) ->
  -- | Fork a thread to run the handler daemon on and
  -- produce a handle to control it.
  IO (Actor message)
spawnStatelessIndividual cleaner interpreter =
  -- TODO: Optimize by reimplementing directly.
  spawnStatefulIndividual () (const cleaner) (const interpreter)

spawnStatelessBatched ::
  (Show message) =>
  -- | Clean up when killed.
  IO () ->
  -- | Interpreter of a batch of messages.
  (NonEmpty message -> IO ()) ->
  -- | Fork a thread to run the handler daemon on and
  -- produce a handle to control it.
  IO (Actor message)
spawnStatelessBatched cleaner interpreter =
  -- TODO: Optimize by reimplementing directly.
  spawnStatefulBatched () (const cleaner) (const interpreter)

spawnStatefulIndividual ::
  (Show message) =>
  state ->
  (state -> IO ()) ->
  (state -> message -> IO state) ->
  IO (Actor message)
spawnStatefulIndividual zero finalizer step =
  spawnStatefulBatched zero finalizer $ foldM step

spawnStatefulBatched ::
  (Show message) =>
  state ->
  (state -> IO ()) ->
  (state -> NonEmpty message -> IO state) ->
  IO (Actor message)
spawnStatefulBatched zero finalizer step =
  do
    runner <- Runner.start
    forkIOWithUnmask $ \unmask ->
      let loop !state =
            do
              messages <- atomically $ Runner.receiveMultiple runner
              case messages of
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
    id <- UuidV4.nextRandom
    return $ fromIdentifiedRunner id runner

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
