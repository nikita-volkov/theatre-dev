{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-binds #-}

module TheatreDev.ActorSpec (spec) where

import Control.Concurrent.Async
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TheatreDev.Actor qualified as Actor
import TheatreDev.ActorSpec.IO qualified as IO
import TheatreDev.ActorSpec.Preferences qualified as Preferences
import Prelude

spec :: Spec
spec =
  do
    describe "kill" do
      describe "When full" do
        it "Does not block" pending
        it "Lets all messages scheduled before be processed" pending
        it "Makes all messages scheduled after be ignored" pending

    describe "wait" do
      describe "When full" do
        it "Blocks until all messages are processed" pending

    describe "spawnStatefulBatched" do
      let spawnIntUpdater step = Actor.spawnStatefulBatched @Int 0 (const (return ())) step
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
        let input = [0 .. 9]
            inputLength = length input

        resultsVar <- newTVarIO []
        actor <- Actor.spawnStatefulBatched [] (const (return ())) $ \state msgs -> do
          let !newState = foldl' (flip (:)) state msgs
          atomically $ writeTVar resultsVar newState
          return newState
        traverse_ (Actor.tell actor) input

        results <- atomically $ do
          results <- readTVar resultsVar
          if length results < inputLength
            then retry
            else return results

        shouldBe (reverse results) input

      it "Kill and wait" do
        let input = [0 .. 9]
            inputLength = length input

        resultVar <- newEmptyMVar
        actor <-
          Actor.spawnStatefulBatched
            []
            ( \state -> do
                threadDelay 1000
                putMVar resultVar state
            )
            ( \state msgs -> return $ foldl' (flip (:)) state msgs
            )
        traverse_ (Actor.tell actor) input

        Actor.kill actor
        Actor.wait actor

        result <- takeMVar resultVar
        shouldBe result $ reverse input

    describe "allOf" . modifyMaxSuccess (max 1000) $ do
      it "Passes 1" do
        let emittersNum = 2
            messagesNum = 10
            actorsNum = 3
            messages = [0 .. messagesNum - 1]
        results <- fmap (fmap sort) (IO.simulateReduction actorsNum emittersNum Actor.allOf messages)
        shouldBe results (replicate actorsNum (sort (concat (replicate emittersNum messages))))
        shouldBe (getSum (foldMap (Sum . length) results)) (actorsNum * emittersNum * messagesNum)
      prop "" $ forAll (chooseInt (0, 99)) $ \size -> forAll arbitrary $ \(messages :: [Int]) ->
        idempotentIOProperty do
          results <- sort . concat <$> IO.simulateReduction Preferences.concurrency size Actor.allOf messages
          return
            $ conjoin
              [ length results === length messages * size * Preferences.concurrency,
                results === sort (concat (replicate (size * Preferences.concurrency) messages))
              ]

    describe "firstAvailableOneOf" . modifyMaxSuccess (max 1000) $ do
      prop "Dispatches correctly" $ forAll (chooseInt (0, 99)) $ \size -> forAll arbitrary $ \(messages :: [Int]) ->
        idempotentIOProperty do
          results <- sort . concat <$> IO.simulateReduction Preferences.concurrency size Actor.firstAvailableOneOf messages
          return
            $ conjoin
              [ length results === length messages * size,
                results
                  === sort (concat (replicate (size) messages))
              ]

    describe "byKeyHashOneOf" . modifyMaxSuccess (max Preferences.largePropertyMaxSuccess) $ do
      prop "Dispatches individually" $ forAll (chooseInt (0, 99)) $ \size -> forAll arbitrary $ \(messages :: [Int]) -> idempotentIOProperty $ do
        resultsVar <- newTVarIO []
        actor <-
          fmap (Actor.byKeyHashOneOf id)
            $ replicateM size
            $ Actor.spawnStatefulIndividual
              IntMap.empty
              ( \state ->
                  atomically
                    $ modifyTVar' resultsVar
                    $ (:) state
              )
              ( \state msg ->
                  return $ IntMap.alter (Just . maybe 1 succ) msg state
              )

        mapConcurrently id
          $ replicate Preferences.concurrency
          $ for_ messages
          $ Actor.tell actor

        Actor.kill actor
        Actor.wait actor

        results <- readTVarIO resultsVar

        let allKeys = results >>= IntMap.keys
            nubbedKeys = nub allKeys

        return
          $ conjoin
            [ allKeys === nubbedKeys,
              if size == 0
                then nubbedKeys === []
                else IntSet.fromList nubbedKeys === IntSet.fromList messages
            ]
