module TheatreDev.Terminal.Spawner where

import TheatreDev.Prelude
import TheatreDev.Terminal.Actor

data Spawner msg = Spawner
  { start :: IO (Actor msg)
  }

pool :: Int -> Spawner msg -> Spawner msg
pool =
  error "TODO"
