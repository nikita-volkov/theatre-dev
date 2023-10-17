module TheatreDev.StatefulActorSpec where

import TheatreDev.Prelude

data StatefulActorSpec message = forall state.
  StatefulActorSpec
  { enter :: Concurrently state,
    step :: state -> NonEmpty message -> Concurrently state,
    exit :: state -> Concurrently ()
  }

instance Semigroup (StatefulActorSpec message) where
  StatefulActorSpec leftEnter leftStep leftExit <> StatefulActorSpec rightEnter rightStep rightExit =
    StatefulActorSpec
      { enter =
          (,) <$> leftEnter <*> rightEnter,
        step = \(leftState, rightState) messages ->
          (,)
            <$> leftStep leftState messages
            <*> rightStep rightState messages,
        exit = \(leftState, rightState) ->
          leftExit leftState *> rightExit rightState
      }

instance Monoid (StatefulActorSpec message) where
  mempty =
    StatefulActorSpec
      { enter = pure (),
        step = const $ const $ pure (),
        exit = const $ pure ()
      }

individual :: IO state -> (state -> message -> IO state) -> (state -> IO ()) -> StatefulActorSpec message
individual enter step exit =
  StatefulActorSpec
    { enter = Concurrently enter,
      step = \state messages -> Concurrently (foldM step state messages),
      exit = \state -> Concurrently (exit state)
    }
