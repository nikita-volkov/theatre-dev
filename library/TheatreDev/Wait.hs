module TheatreDev.Wait where

import TheatreDev.Prelude

type Wait = STM (Maybe SomeException)

both :: Wait -> Wait -> Wait
both left right =
  liftA2 (<|>) left right

all :: [Wait] -> Wait
all waits =
  asum <$> sequence waits
