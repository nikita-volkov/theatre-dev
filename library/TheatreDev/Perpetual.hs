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
-- Actor, which processes the messages of type @event@.
--
-- Provides abstraction over the communication channel and threads.
newtype Actor event
  = Actor (event -> IO ())

instance Semigroup (Actor event) where
  Actor lTell <> Actor rTell =
    Actor $ \event -> lTell event >> rTell event

instance Monoid (Actor event) where
  mempty =
    Actor (const (return ()))

spawn ::
  -- | Initial state.
  state ->
  -- | Process the next event updating the state.
  (state -> event -> IO state) ->
  IO (Actor event)
spawn state process = do
  (inChan, outChan) <- Unagi.newChan
  forkIO $
    let loop !state = do
          event <- Unagi.readChan outChan
          state <- process state event
          loop state
     in loop state
  return $ Actor $ Unagi.writeChan inChan

-- |
-- Schedule a message for the actor to process
-- after the ones already scheduled.
tell :: Actor event -> event -> IO ()
tell = coerce
