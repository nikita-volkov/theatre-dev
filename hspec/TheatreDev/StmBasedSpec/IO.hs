module TheatreDev.StmBasedSpec.IO where

import Control.Concurrent.Async
import TheatreDev.StmBased (Actor)
import TheatreDev.StmBased qualified as Actor
import Prelude

simulateReduction :: Int -> Int -> ([Actor a] -> Actor a) -> [a] -> IO [[a]]
simulateReduction actorsNum generatorsNum reducer messages =
  do
    resultsVar <- newTVarIO []

    actor <-
      fmap reducer
        $ replicateM actorsNum
        $ Actor.spawnStatefulIndividual
          []
          ( \state ->
              atomically
                $ modifyTVar' resultsVar (reverse state :)
          )
          ( \state msg ->
              return $ msg : state
          )

    mapConcurrently id
      $ replicate generatorsNum
      $ for_ messages
      $ Actor.tell actor

    Actor.kill actor
    Actor.wait actor

    readTVarIO resultsVar
