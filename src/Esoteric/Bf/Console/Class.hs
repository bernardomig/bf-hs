module Esoteric.Bf.Console.Class (MonadConsole (..)) where

class (Monad m) => MonadConsole m where
  input :: m Int
  output :: Int -> m ()