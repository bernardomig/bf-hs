module Esoteric.Bf.Tape.Class where

class (Monad m) => MonadTape m where
  forward :: m ()
  back :: m ()
  write :: Int -> m ()
  read :: m Int