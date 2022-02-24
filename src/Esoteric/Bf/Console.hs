{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Esoteric.Bf.Console where

import Control.Monad.State (MonadState (state), StateT (runStateT), execStateT, modify)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (second)
import Esoteric.Bf.Console.Class (MonadConsole (..))

newtype Console m b = MkConsole {unConsole :: StateT ([Int], [Int]) m b}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadState ([Int], [Int])
    )

instance MonadTrans Console where
  lift = MkConsole . lift

instance (Monad m) => MonadConsole (Console m) where
  input = MkConsole $
    state $ \(input, output) -> case input of
      [] -> (0, (input, output))
      (x : xs) -> (x, (xs, output))
  output c = MkConsole $ modify $ second (c :)

execConsole :: (Monad m) => [Int] -> Console m a -> m [Int]
execConsole input f = reverse . snd <$> execStateT (unConsole f) (input, [])
