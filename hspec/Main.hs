module Main (main) where

import Test.Hspec
import TheatreDev.ActorSpec qualified
import Prelude

main :: IO ()
main =
  hspec do
    describe "TheatreDev.Actor" TheatreDev.ActorSpec.spec
