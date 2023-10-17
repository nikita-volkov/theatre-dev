module TheatreDev.StatefulActorSpec where

import TheatreDev.Prelude

data StatefulActorSpec message = forall state.
  StatefulActorSpec
  { enter :: IO state,
    step :: state -> NonEmpty message -> IO state,
    exit :: state -> IO ()
  }

instance Semigroup (StatefulActorSpec message) where
  StatefulActorSpec leftEnter leftStep leftExit <> StatefulActorSpec rightEnter rightStep rightExit =
    StatefulActorSpec
      { enter = (,) <$> leftEnter <*> rightEnter,
        step = \(leftState, rightState) messages ->
          runConcurrently
            $ (,)
            <$> Concurrently (leftStep leftState messages)
            <*> Concurrently (rightStep rightState messages),
        exit = \(leftState, rightState) ->
          runConcurrently $ do
            Concurrently (leftExit leftState)
            Concurrently (rightExit rightState)
            return ()
      }

individual :: IO state -> (state -> message -> IO state) -> (state -> IO ()) -> StatefulActorSpec message
individual enter step exit =
  StatefulActorSpec
    { enter,
      step = foldM step,
      exit
    }
