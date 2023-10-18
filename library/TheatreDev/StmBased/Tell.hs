module TheatreDev.StmBased.Tell where

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
