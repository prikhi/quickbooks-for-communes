{- | This module contains simple helper functions used throughout the
application.
-}
module Utils where


-- | Traverse a list, applying a function that requires each item's index.
traverseWithIndex :: Applicative f => (Int -> a -> f b) -> [a] -> f [b]
traverseWithIndex func = run 0
  where
    run c ys = case ys of
        []     -> pure []
        x : xs -> (:) <$> func c x <*> run (c + 1) xs
