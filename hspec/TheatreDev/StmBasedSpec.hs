{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module TheatreDev.StmBasedSpec (spec) where

import Test.Hspec
import qualified TheatreDev.StmBased as Actor
import Prelude

spec :: Spec
spec =
  do
    describe "spawnStatelessBatched" do
      let spawnInt step = Actor.spawnStatefulBatched @Int 0 (const (return ())) step
      let spawnUnit step = Actor.spawnStatefulBatched () (const (return ())) step

      it "Works in batches" do
        acc <- newIORef []
        actorLock <- newEmptyMVar
        emitterLock <- newEmptyMVar
        actor <- spawnUnit $ \_ messages ->
          do
            modifyIORef' acc (messages :)
            putMVar emitterLock ()
            takeMVar actorLock

        Actor.tell actor 1

        takeMVar emitterLock
        Actor.tell actor 2
        Actor.tell actor 3
        putMVar actorLock ()

        takeMVar emitterLock
        Actor.tell actor 4
        putMVar actorLock ()

        takeMVar emitterLock

        collectedBatches <- reverse . fmap toList <$> readIORef acc
        shouldBe collectedBatches [[1], [2, 3], [4]]

      it "Threads the state" do
        pending

      it "Kill and wait" do
        pending
