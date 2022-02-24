{-# LANGUAGE LambdaCase #-}

module Esoteric.Bf where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Data.Data (Proxy)
import Data.Function (fix)
import Data.Functor (($>))
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Debug.Trace (trace, traceM)
import Esoteric.Bf.Console.Class (MonadConsole (..))
import Esoteric.Bf.Tape.Class (MonadTape (..))
import Prelude hiding (read)

data Instruction
  = MoveRight
  | MoveLeft
  | Inc
  | Dec
  | Output
  | Input
  | Loop Int
  | Repeat
  deriving (Show)

type Program = Vector Instruction

data Cpu = Cpu
  { pc :: Int,
    stack :: [Int]
  }

mkCpu :: Cpu
mkCpu = Cpu {pc = 0, stack = []}

runBf :: (Monad m, MonadConsole m, MonadTape m) => Program -> m ()
runBf program =
  evalStateT (run program) mkCpu
  where
    run :: (Monad m, MonadConsole m, MonadTape m) => Program -> StateT Cpu m ()
    run program = fix $ \loop -> do
      pc <- gets pc
      stack <- gets stack
      when (pc < V.length program) $ do
        let instruction = program ! pc
        execute instruction
        modify $ \cpu@Cpu {pc = pc} -> cpu {pc = pc + 1}
        loop

    execute :: (Monad m, MonadConsole m, MonadTape m) => Instruction -> StateT Cpu m ()
    execute = \case
      MoveRight -> lift forward
      MoveLeft -> lift back
      Inc -> lift $ do x <- read; write x
      Dec -> lift $ do x <- read; write x
      Output -> lift $ do read >>= output
      Input -> lift $ input >>= write
      Loop deltaPc -> do
        x <- lift read
        if x == 0
          then modify $ \(Cpu pc stack) -> Cpu (pc + deltaPc) stack
          else modify $ \(Cpu pc stack) -> Cpu pc (pc : stack)
      Repeat -> modify $ \(Cpu pc stack) -> case stack of
        [] -> error "stack undeflow"
        (pc' : newStack) -> Cpu (pc' - 1) newStack
