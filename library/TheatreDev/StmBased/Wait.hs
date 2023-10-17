module TheatreDev.StmBased.Wait where

import TheatreDev.Prelude

type Wait = STM (Maybe SomeException)

both :: Wait -> Wait -> Wait
both left right =
  do
    firstResult <- Left <$> left <|> Right <$> right
    case firstResult of
      Left Nothing -> right
      Left (Just exception) -> return (Just exception)
      Right Nothing -> left
      Right (Just exception) -> return (Just exception)

all :: [Wait] -> Wait
all waits =
  getException <|> getNothing
  where
    getException =
      waits
        & fmap (>>= maybe empty pure)
        & asum
        & fmap Just
    getNothing =
      Nothing <$ sequence_ waits
