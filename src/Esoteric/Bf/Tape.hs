{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}

module Esoteric.Bf.Tape where

import Control.Monad.Primitive (MonadPrim, PrimMonad (PrimState))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState (..), modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Primitive (MutableArray, newArray, readArray, writeArray)
import Esoteric.Bf.Console (Console)
import Esoteric.Bf.Tape.Class (MonadTape (..))
import Prelude hiding (read)

newtype FixedTape s m b = MkFixedTape {unFixedTape :: ReaderT (MutableArray s Int) (StateT Int m) b}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadReader (MutableArray s Int),
      MonadState Int
    )

instance MonadTrans (FixedTape s) where
  lift = MkFixedTape . lift . lift

instance (MonadPrim s m) => MonadTape (FixedTape s m) where
  forward = modify (+ 1)
  back = modify (`subtract` 1)
  write c = do
    mem <- ask
    ptr <- get
    lift $ writeArray mem ptr c
  read = do
    mem <- ask
    ptr <- get
    lift $ readArray mem ptr

runFixedTape :: (PrimMonad m) => Int -> FixedTape (PrimState m) m b -> m b
runFixedTape size f = do
  arr <- newArray size 0
  evalStateT (runReaderT (unFixedTape f) arr) 0

instance MonadTape m => MonadTape (Console m) where
  write c = lift (write c)
  read = lift read
  forward = lift forward
  back = lift back
