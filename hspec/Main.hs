module Main (main) where

import Test.Hspec
import TheatreDevSpec qualified
import Prelude

main :: IO ()
main =
  hspec do
    describe "TheatreDev" TheatreDevSpec.spec
