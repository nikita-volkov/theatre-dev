module TheatreDev.Terminal
  ( Actor,

    -- * Manipulation
    batchify,

    -- * Acquisition
    spawnStateless,
    spawnStateful,

    -- * Usage
    tell,
    kill,
    wait,
  )
where

import qualified Control.Concurrent.Chan.Unagi as E
import qualified SlaveThread as F
import TheatreDev.Prelude

-- |
-- Actor, which processes the messages of type @message@.
--
-- An abstraction over the message channel, thread-forking and killing.
data Actor message = Actor
  { -- | Send a message to the actor.
    tell :: message -> IO (),
    -- | Kill the actor.
    kill :: IO (),
    -- | Wait for the actor to die due to error or being killed.
    wait :: IO ()
  }

instance Semigroup (Actor message) where
  (<>) (Actor lTell lKill lWait) (Actor rTell rKill rWait) =
    Actor tell kill wait
    where
      tell msg = lTell msg >> rTell msg
      kill = lKill >> rKill
      wait = lWait >> rWait

instance Monoid (Actor message) where
  mempty =
    Actor (const (return ())) (return ()) (return ())

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
      wait = lWait >> rWait

instance Decidable Actor where
  lose fn =
    Actor (const (return ()) . absurd . fn) (return ()) (return ())
  choose choice (Actor lTell lKill lWait) (Actor rTell rKill rWait) =
    Actor tell kill wait
    where
      tell = either lTell rTell . choice
      kill = lKill >> rKill
      wait = lWait >> rWait

-- |
-- Adapt the actor to be able to receive lists of messages.
batchify :: Actor message -> Actor [message]
batchify Actor {..} =
  Actor (traverse_ tell) kill wait

-- |
-- An actor which cannot die by itself unless explicitly killed.
--
-- Given an interpreter of messages,
-- forks a thread to run the computation on and
-- produces a handle to address that actor.
--
-- Killing that actor will make it process all the messages in the queue first.
-- All the messages sent to it after killing won't be processed.
spawnStateless ::
  -- | Interpreter of a message
  (message -> IO ()) ->
  -- | Clean up when killed
  (IO ()) ->
  IO (Actor message)
spawnStateless interpretMessage cleanUp =
  do
    (inChan, outChan) <- E.newChan
    lock <- newEmptyMVar
    spawningThreadId <- myThreadId
    forkIO $
      let loop =
            {-# SCC "spawnStateless/loop" #-}
            do
              message <- E.readChan outChan
              case message of
                Just payload ->
                  do
                    res <- try @SomeException $ interpretMessage payload
                    case res of
                      Right () -> loop
                      Left exc ->
                        do
                          cleanUp
                          putMVar lock ()
                          throwTo spawningThreadId exc
                Nothing ->
                  do
                    cleanUp
                    putMVar lock ()
       in loop
    return
      ( Actor
          (E.writeChan inChan . Just)
          (E.writeChan inChan Nothing)
          (takeMVar lock)
      )

-- |
-- Actor with memory.
--
-- Threads a persistent state thru its iterations.
--
-- Given an interpreter of messages and initial state generator,
-- forks a thread to run the computation on and
-- produces a handle to address that actor.
--
-- Killing that actor will make it process all the messages in the queue first.
-- All the messages sent to it after killing won't be processed.
spawnStateful :: state -> (state -> message -> IO state) -> (state -> IO ()) -> IO (Actor message)
spawnStateful state progress finalise =
  do
    (inChan, outChan) <- E.newChan
    lock <- newEmptyMVar
    F.fork $
      let {-# SCC loop #-}
          loop !state = do
            message <- E.readChan outChan
            case message of
              Just message -> do
                state <- progress state message
                loop state
              Nothing -> do
                finalise state
                putMVar lock ()
       in loop state
    return
      ( Actor
          (E.writeChan inChan . Just)
          (E.writeChan inChan Nothing)
          (takeMVar lock)
      )

spawnStatefulBatched :: state -> (state -> NonEmpty message -> IO state) -> (state -> IO ()) -> IO (Actor message)
spawnStatefulBatched =
  error "TODO"
