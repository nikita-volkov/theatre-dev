module Main (main) where

import Test.Hspec
import TheatreDev.StmBasedSpec qualified
import Prelude

main :: IO ()
main =
  hspec do
    describe "TheatreDev" do
      describe "StmBased" TheatreDev.StmBasedSpec.spec
