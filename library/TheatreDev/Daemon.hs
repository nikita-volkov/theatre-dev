module TheatreDev.Daemon
  ( Daemon,

    -- * Acquisition
    spawn,

    -- * Control
    kill,
    wait,
  )
where

import TheatreDev.Prelude
import TheatreDev.Wait qualified as Wait

data Config = forall state.
  Config
  { initialState :: state,
    iterate :: state -> IO state,
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
                  atomically (writeTMVar resultVar (Just exception))
            else do
              cleanUpResult <- try @SomeException (unmask (cleanUp state))
              case cleanUpResult of
                Right () -> atomically (writeTMVar resultVar Nothing)
                Left exception -> atomically (writeTMVar resultVar (Just exception))
     in go initialState
  return
    Daemon
      { kill = writeTVar iteratingVar False,
        wait = readTMVar resultVar
      }
  where

kill :: Daemon -> IO ()
kill daemon =
  atomically daemon.kill

wait :: Daemon -> IO ()
wait daemon =
  atomically daemon.wait >>= maybe (pure ()) throwIO
