{-# LANGUAGE LambdaCase #-}

module Stack
    ( Instruction(..)
    , parse
    , StackParseException
    ) where

import Data.Char (isSpace)

data StackParseException
    = InvalidInstructionException String
    | OtherStackException
    deriving (Show)

data Instruction
    = Push Double
    | Pop
    | Add
    | Print
    | Debug
    deriving (Show)

parse :: String -> Either StackParseException [Instruction]
parse = traverse (go . words) . lines
  where
    go :: [String] -> Either StackParseException Instruction
    go =
        \case
            ["pop"] -> Right Pop
            ["add"] -> Right Add
            ["print"] -> Right Print
            ["debug"] -> Right Debug
            ["hcs-debug"] -> Right Debug
            ["push", xs] -> Right $ Push (read xs :: Double)
            [other] -> Left $ InvalidInstructionException other
            other -> Left OtherStackException
