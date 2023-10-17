module TheatreDev.ExtrasFor.List where

import TheatreDev.Prelude

splitWhileJust :: [Maybe a] -> ([a], [Maybe a])
splitWhileJust = go []
  where
    go !acc = \case
      head : tail -> case head of
        Nothing -> (reverse acc, head : tail)
        Just liveHead -> go (liveHead : acc) tail
      [] -> (reverse acc, [])
