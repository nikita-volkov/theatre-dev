module TheatreDev.Daemon
  ( Daemon,

    -- * Acquisition
    Config (..),
    spawn,

    -- * Control
    kill,
    wait,
  )
where

import TheatreDev.Prelude
import TheatreDev.Wait qualified as Wait

-- | Configuration of the daemon behaviour.
data Config = forall state.
  Config
  { 
    -- | Initial state of the daemon.
    initialState :: state,
    -- | Iteration action, updating the daemon's state.
    -- It gets executed in a loop,
    -- with checks of whether the daemon is still alive after each one.
    -- Killing the daemon will not interrupt the currently ongoing iteration,
    -- thus providing gracefulness guarantees.
    -- 
    -- If an exception is thrown by this action,
    -- the iteration loop will stop,
    -- the 'cleanUp' action will get executed and
    -- in all place where 'wait' is called the exception will be rethrown.
    iterate :: state -> IO state,
    -- | Clean up after the iteration loop is stopped.
    -- You can use that to release resources or
    -- issue notifications about the daemon dying.
    cleanUp :: state -> IO ()
  }

-- |
-- Think of an actor that does not process any messages and simply
-- interrupts between each iteration to check whether it's still alive.
data Daemon = Daemon
  { -- | Kill the daemon.
    kill :: STM (),
    -- | Wait for the daemon to die due to error or being killed.
    wait :: STM (Maybe SomeException)
  }

instance Semigroup Daemon where
  left <> right =
    Daemon
      { kill = left.kill *> right.kill,
        wait = Wait.both left.wait right.wait
      }

instance Monoid Daemon where
  mempty =
    Daemon
      { kill = return (),
        wait = return Nothing
      }
  mconcat daemons =
    Daemon
      { kill = traverse_ (.kill) daemons,
        wait = Wait.all (fmap (.wait) daemons)
      }

-- | Fork a thread to run the daemon loop on
-- returning immediately with a handle to control it.
spawn :: Config -> IO Daemon
spawn Config {..} = do
  iteratingVar <- newTVarIO True
  resultVar <- newEmptyTMVarIO
  forkIOWithUnmask $ \unmask ->
    let go !state = do
          iterating <- readTVarIO iteratingVar
          if iterating
            then do
              iterationAttemptResult <- try (unmask (iterate state))
              case iterationAttemptResult of
                Right newState -> go newState
                Left exception -> do
                  try @SomeException (unmask (cleanUp state))
                  atomically (putTMVar resultVar (Just exception))
            else do
              cleanUpResult <- try @SomeException (unmask (cleanUp state))
              case cleanUpResult of
                Right () -> atomically (putTMVar resultVar Nothing)
                Left exception -> atomically (putTMVar resultVar (Just exception))
     in go initialState
  return
    Daemon
      { kill = writeTVar iteratingVar False,
        wait = readTMVar resultVar
      }
  where

-- | Command the daemon to stop iterating,
-- finish the ongoing iteration and execute the clean up action.
--
-- This action executes immediately.
-- If you want to block waiting for the daemon to actually die,
-- after 'kill' you can run 'wait'.
kill :: Daemon -> IO ()
kill daemon =
  atomically daemon.kill

-- | Block waiting for the daemon to die either due to getting killed
-- or due to its iterator action throwing an exception.
-- The exception will get rethrown here.
wait :: Daemon -> IO ()
wait daemon =
  atomically daemon.wait >>= maybe (pure ()) throwIO