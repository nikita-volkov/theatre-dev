module TheatreDev.Tell where

import Data.Vector qualified as Vector
import TheatreDev.Prelude

type Tell a = a -> STM ()

either :: Tell a -> Tell a -> Tell a
either lTell rTell msg =
  lTell msg <|> rTell msg

both :: Tell a -> Tell a -> Tell a
both lTell rTell msg =
  lTell msg >> rTell msg

one :: [Tell a] -> Tell a
one tells msg =
  asum $ fmap (\tell -> tell msg) tells

all :: [Tell a] -> Tell a
all tells msg =
  traverse_ (\tell -> tell msg) tells

byKeyHashOneOf :: (a -> Int) -> [Tell a] -> Tell a
byKeyHashOneOf proj tells =
  let vector = Vector.fromList tells
      vectorLength = Vector.length vector
   in case vectorLength of
        0 -> const (pure ())
        _ -> \msg ->
          let index = mod (proj msg) vectorLength
              tellAtIndex = Vector.unsafeIndex vector index
           in tellAtIndex msg
