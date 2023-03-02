-- |
-- Exploration of perpetual actors.
-- I.e., those that exist for the whole duration of the app.
--
-- This limitation provides for simpler API and most apps
-- are expected not to need more.
module TheatreDev.Perpetual
  ( Actor,
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

-- |
-- Distributes the message across the merged actors.
instance Semigroup (Actor msg) where
  Actor lTell <> Actor rTell =
    Actor $ \msg -> lTell msg >> rTell msg
  sconcat actors = Actor $ \msg -> forM_ actors $ \(Actor tell) -> tell msg
  stimes n (Actor tell) = Actor $ \msg -> replicateM_ (fromIntegral n) $ tell msg

-- |
-- Provides an identity for merging the actors,
-- which does nothing.
instance Monoid (Actor msg) where
  mempty = Actor (const (return ()))
  mconcat actors = Actor $ \msg -> forM_ actors $ \(Actor tell) -> tell msg

-- |
-- Maps the input message to a different type.
instance Contravariant Actor where
  contramap fn (Actor tell) =
    Actor (tell . fn)

-- |
-- Splits the message between actors.
instance Divisible Actor where
  conquer =
    mempty
  divide divisor (Actor lTell) (Actor rTell) =
    Actor $ \msg -> case divisor msg of
      (lMsg, rMsg) -> lTell lMsg >> rTell rMsg

-- |
-- Provides a choice between alternative actors to process the message.
instance Decidable Actor where
  lose _ =
    Actor $ const $ return ()
  choose decider (Actor lTell) (Actor rTell) =
    Actor $ either lTell rTell . decider

spawn ::
  -- |
  -- Initial state.
  state ->
  -- |
  -- Process the next message updating the state.
  -- The IO action must not throw any exceptions.
  (state -> msg -> IO state) ->
  -- |
  -- Action forking a thread to run the actor loop and
  -- producing a handle for sending messages to it.
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
