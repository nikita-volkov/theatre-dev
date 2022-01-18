-- |
-- Exploration of perpetual actors.
-- I.e., those that exist for the whole duration of the app.
--
-- This limitation provides for simpler API and most apps
-- are expected not to need more.
module TheatreDev.Perpetual
  ( -- *
    Actor,

    -- **
    spawn,
    tell,
  )
where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import TheatreDev.Prelude

-- |
-- Actor, which processes the messages of type @msg@.
--
-- Provides abstraction over the communication channel and threads.
newtype Actor msg
  = Actor (msg -> IO ())

instance Semigroup (Actor msg) where
  Actor lTell <> Actor rTell =
    Actor $ \msg -> lTell msg >> rTell msg

instance Monoid (Actor msg) where
  mempty =
    Actor (const (return ()))

spawn ::
  -- | Initial state.
  state ->
  -- | Process the next message updating the state.
  (state -> msg -> IO state) ->
  IO (Actor msg)
spawn state process = do
  (inChan, outChan) <- Unagi.newChan
  forkIO $
    let loop !state = do
          msg <- Unagi.readChan outChan
          state <- process state msg
          loop state
     in loop state
  return $ Actor $ Unagi.writeChan inChan

-- |
-- Schedule a message for the actor to process
-- after the ones already scheduled.
tell :: Actor msg -> msg -> IO ()
tell = coerce
