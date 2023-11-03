module TheatreDev.Daemon where

import TheatreDev.Prelude

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
  { iteratingVar :: TVar Bool,
    resultVar :: TMVar (Maybe SomeException)
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
                Right () -> return ()
                Left exception -> do
                  atomically (writeTMVar resultVar (Just exception))
     in go initialState
  return Daemon {..}
  where

kill :: Daemon -> IO ()
kill =
  error "TODO"

wait :: Daemon -> IO ()
wait =
  error "TODO"
