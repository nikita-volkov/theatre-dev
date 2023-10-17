module TheatreDev.Terminal.Actor
  ( Actor,

    -- * Manipulation
    adaptToList,

    -- * Acquisition
    spawnStatelessGranular,
    spawnStatefulGranular,
    spawnStatefulBatched,

    -- * Control
    tell,
    kill,
    wait,
  )
where

import qualified Control.Concurrent.Chan.Unagi as E
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TMVar
import qualified TheatreDev.ExtrasFor.List as List
import TheatreDev.ExtrasFor.TBQueue
import TheatreDev.Prelude

-- |
-- Controls of an actor, which processes the messages of type @message@.
--
-- Abstraction over the message channel, thread-forking and killing.
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
adaptToList :: Actor message -> Actor [message]
adaptToList Actor {..} =
  case traverse_ tell of
    tell -> Actor {..}

-- |
-- Given an interpreter of messages,
-- fork a thread to run the handler daemon on and
-- produce a handle to control that actor.
--
-- Killing that actor will make it process all the messages in the queue first.
-- All the messages sent to it after killing won't be processed.
spawnStatelessGranular ::
  -- | Interpreter of a message.
  (message -> IO ()) ->
  -- | Clean up when killed.
  IO () ->
  -- | Fork a thread to run the handler daemon on and
  -- produce a handle to control it.
  IO (Actor message)
spawnStatelessGranular interpretMessage cleanUp =
  do
    (inChan, outChan) <- E.newChan
    lock <- newEmptyMVar
    spawningThreadId <- myThreadId
    forkIO
      $ let loop =
              {-# SCC "spawnStatelessGranular/loop" #-}
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
spawnStatefulGranular :: state -> (state -> message -> IO state) -> (state -> IO ()) -> IO (Actor message)
spawnStatefulGranular zero step finalizer =
  spawnStatefulBatched zero newStep finalizer
  where
    newStep =
      foldM step

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
                -- Automatically means that the tail is not empty.
                [] -> do
                  writeTVar aliveVar False
                  putTMVar resVar Nothing
                  return $ do
                    finalizer state
                messagesHead : messagesTail ->
                  return $ do
                    result <- try @SomeException $ unmask $ step state (messagesHead :| messagesTail)
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
        { tell = \message -> atomically $ do
            alive <- readTVar aliveVar
            when alive
              $ writeTBQueue queue
              $ Just message,
          kill = atomically $ do
            alive <- readTVar aliveVar
            when alive
              $ writeTBQueue queue Nothing,
          wait = do
            res <- atomically $ takeTMVar resVar
            case res of
              Nothing -> return ()
              Just exception -> throwIO exception
        }
