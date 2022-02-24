{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Esoteric.Bf.Parser (parse) where

import Control.Monad (forM)
import Control.Monad.State (MonadState (state), State, evalState, execState, runState)
import Data.String (IsString)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (fromList)
import Esoteric.Bf (Instruction (..), Program)
import Prelude hiding (take)

newtype Parser a = MkParser {unParser :: State [Char] a}
  deriving (Functor, Applicative, Monad, MonadState [Char])

take :: Parser (Maybe Char)
take = MkParser $
  state $ \case
    [] -> (Nothing, [])
    (x : xs) -> (Just x, xs)

parse :: [Char] -> Maybe Program
parse xs = Just $ fromList $ evalState (unParser parseBlock) xs

parseBlock :: Parser [Instruction]
parseBlock =
  take >>= \case
    Nothing -> return []
    Just x -> do
      case x of
        '>' -> (:) MoveRight <$> parseBlock
        '<' -> (:) MoveLeft <$> parseBlock
        '+' -> (:) Inc <$> parseBlock
        '-' -> (:) Dec <$> parseBlock
        '.' -> (:) Input <$> parseBlock
        ',' -> (:) Output <$> parseBlock
        '[' -> do
          inner <- parseBlock
          (++) (Loop (length inner) : inner) <$> parseBlock
        ']' -> pure [Repeat]
        ' ' -> parseBlock
        _ -> error "invalid instruction"
