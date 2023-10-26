module TheatreDev
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
    firstAvailableOneOf,
    byKeyHashOneOf,
    allOf,
  )
where

import TheatreDev.Prelude
import TheatreDev.StmStructures.Runner (Runner)
import TheatreDev.StmStructures.Runner qualified as Runner
import TheatreDev.Tell (Tell)
import TheatreDev.Tell qualified as Tell
import TheatreDev.Wait qualified as Wait

-- |
-- Controls of an actor, which processes the messages of type @message@.
-- The processing runs on a dedicated green thread.
--
-- Provides abstraction over the message channel, thread-forking and killing.
--
-- Monoid instance is not provided for the same reason it is not provided for numbers.
-- This type supports both sum and product composition. See 'allOf', 'firstAvailableOneOf' and 'byKeyHashOneOf'.
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

fromRunner :: Runner a -> Actor a
fromRunner runner =
  Actor
    { tell = Runner.tell runner,
      kill = Runner.kill runner,
      wait = Runner.wait runner,
      ids = [Runner.getId runner]
    }

-- | Distribute the message stream across actors.
-- The message gets delivered to the first available one.
--
-- E.g., using this combinator in combination with 'replicateM'
-- you can construct pools:
--
-- > spawnPool :: Int -> IO (Actor message) -> IO (Actor message)
-- > spawnPool size spawn =
-- >   firstAvailableOneOf <$> replicateM size spawn
--
-- You can consider this being an interface to the Sum monoid.
firstAvailableOneOf :: [Actor message] -> Actor message
firstAvailableOneOf = tellComposition Tell.one

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
byKeyHashOneOf ::
  -- | Function extracting the key from the message and hashing it.
  (message -> Int) ->
  -- | Pool of actors.
  [Actor message] ->
  Actor message
byKeyHashOneOf = tellComposition . Tell.byKeyHashOneOf

-- | Distribute the message stream to all provided actors.
--
-- You can consider this being an interface to the Product monoid.
allOf :: [Actor message] -> Actor message
allOf = tellComposition Tell.all

tellComposition :: ([Tell message] -> Tell message) -> [Actor message] -> Actor message
tellComposition tellReducer actors =
  Actor
    { tell = tellReducer (fmap (.tell) actors),
      kill = traverse_ (.kill) actors,
      wait = Wait.all (fmap (.wait) actors),
      ids = foldMap (.ids) actors
    }

-- * Acquisition

-- | Spawn an actor which processes messages in isolated executions.
spawnStatelessIndividual ::
  -- | Clean up when killed or exception is thrown.
  IO () ->
  -- | Interpret a message.
  (message -> IO ()) ->
  -- | Fork a thread to run the handler loop on and produce a handle to control it.
  IO (Actor message)
spawnStatelessIndividual cleaner interpreter =
  -- TODO: Optimize by reimplementing directly.
  spawnStatefulIndividual () (const cleaner) (const interpreter)

-- | Spawn an actor which processes all available messages in one execution.
spawnStatelessBatched ::
  -- | Clean up when killed or exception is thrown.
  IO () ->
  -- | Interpret a batch of messages.
  (NonEmpty message -> IO ()) ->
  -- | Fork a thread to run the handler loop on and produce a handle to control it.
  IO (Actor message)
spawnStatelessBatched cleaner interpreter =
  -- TODO: Optimize by reimplementing directly.
  spawnStatefulBatched () (const cleaner) (const interpreter)

-- | Spawn an actor which processes messages in isolated executions
-- and threads state.
spawnStatefulIndividual ::
  -- | Initial state.
  state ->
  -- | Clean up when killed or exception is thrown.
  (state -> IO ()) ->
  -- | Process a message and update state.
  (state -> message -> IO state) ->
  -- | Fork a thread to run the handler loop on and produce a handle to control it.
  IO (Actor message)
spawnStatefulIndividual zero finalizer step =
  spawnStatefulBatched zero finalizer $ foldM step

-- | Spawn an actor which processes all available messages in one execution
-- and threads state.
spawnStatefulBatched ::
  -- | Initial state.
  state ->
  -- | Clean up when killed or exception is thrown.
  (state -> IO ()) ->
  -- | Process a batch of messages and update state.
  (state -> NonEmpty message -> IO state) ->
  -- | Fork a thread to run the handler loop on and produce a handle to control it.
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
                    result <- try $ unmask $ step state nonEmptyMessages
                    case result of
                      Right newState ->
                        loop newState
                      Left exception ->
                        finally (finalizer state)
                          $ atomically
                          $ Runner.releaseWithException runner exception
                -- Empty batch means that the runner is finished.
                Nothing ->
                  finally (finalizer state)
                    $ atomically
                    $ Runner.releaseNormally runner
       in loop zero
    return $ fromRunner runner

-- * Control

-- | Add a message to the end of the queue of the
-- messages to be processed by the provided actor.
tell :: Actor message -> message -> IO ()
tell actor =
  atomically . actor.tell

-- | Command the actor to stop registering new messages,
-- process all the registered ones and execute the clean up action.
--
-- This action executes immediately.
-- If you want to block waiting for the actor to actually die,
-- after 'kill' you can run 'wait'.
kill :: Actor message -> IO ()
kill actor =
  atomically actor.kill

-- | Block waiting for the actor to die either due to getting killed
-- or due to its interpreter action throwing an exception.
-- The exception will get rethrown here.
wait :: Actor message -> IO ()
wait actor =
  atomically actor.wait >>= maybe (pure ()) throwIO
