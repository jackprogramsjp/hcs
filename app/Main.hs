{-# LANGUAGE BlockArguments #-}

module Main where

import Codegen
import Control.Monad (when)
import Lib
import Stack
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Printf (printf)

input :: String -> IO String
input str = putStr str *> hFlush stdout *> getLine

checkArgsIndex :: String -> [String] -> Int -> String
checkArgsIndex command args i =
    if length args < i
        then error $ "Argument needed for command '" ++ command ++ "'"
        else args !! i

checkInstructions ::
       Either StackParseException [Instruction] -> IO [Instruction]
checkInstructions ins =
    case ins of
        Left e -> hPrint stderr e >> hFlush stderr >> exitFailure
        Right i -> return i

usage :: String
usage = "usage: hcs [version] [run <filename>] [dump <filename>]"

main :: IO ()
main = do
    args <- getArgs
    when (null args) do putStrLn usage >> exitSuccess
    let command = head args
    case command of
        "run" -> do
            contents <- readFile $ checkArgsIndex command args 1
            instructions <- checkInstructions $ Stack.parse contents
            Codegen.defaultRun instructions
        "version" -> putStrLn "hcs 0.1.0"
        "dump" -> do
            contents <- readFile $ checkArgsIndex command args 1
            instructions <- checkInstructions $ Stack.parse contents
            putStrLn $ Codegen.defaultCompile instructions
        _ -> printf "Unknown command '%s'\n%s\n" command usage
