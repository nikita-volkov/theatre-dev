{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module TheatreDev.StmBasedSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
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
        Actor.kill actor

        collectedBatches <- reverse . fmap toList <$> readIORef acc
        shouldBe collectedBatches [[1], [2, 3], [4]]

      it "Threads the state" do
        pending

      it "Kill and wait" do
        pending

    describe "byKeyHash" . modifyMaxSuccess (max 10000) $ do
      prop "Dispatches individually" $ forAll (chooseInt (0, 99)) $ \size -> forAll arbitrary $ \messages -> monadicIO $ do
        results <- liftIO do
          resultQueue <- newTQueueIO
          actor <-
            fmap (Actor.byKeyHash id)
              $ replicateM size
              $ Actor.spawnStatefulIndividual
                IntMap.empty
                ( \state ->
                    atomically $ writeTQueue resultQueue state
                )
                ( \state msg ->
                    return $ IntMap.alter (Just . maybe 1 succ) msg state
                )

          for_ @[] messages $ Actor.tell actor

          Actor.kill actor
          Actor.wait actor

          atomically $ flushTQueue resultQueue

        let allKeys = results >>= IntMap.keys
            nubbedKeys = nub allKeys

        return
          $ conjoin
            [ allKeys === nubbedKeys,
              if size == 0
                then nubbedKeys === []
                else IntSet.fromList nubbedKeys === IntSet.fromList messages
            ]
