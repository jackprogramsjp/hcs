{-# LANGUAGE BlockArguments #-}

module Codegen
    ( start
    , gen
    , run
    , defaultRun
    , defaultCompile
    ) where

import Control.Monad (unless)
import Stack

spaces :: Int -> String
spaces n
    | n <= 0 = ""
    | otherwise = replicate n ' '

-- Does not format macros
formatC :: Int -> String -> String
formatC tabSize code = spaces tabSize ++ format tabSize tabSize code []
    -- TabNumber = The Original number of tabs: Never changes
    -- TabSize = The number of tabs to CURRENTLY put in: Always changes
  where
    format :: Int -> Int -> String -> String -> String
    format tabSize tabNumber code newCode =
        case code of
            [] -> reverse newCode
            (c:cs) ->
                let tabs = spaces tabSize
                 in case c of
                        '{' ->
                            format (tabSize + tabNumber) tabNumber cs $
                            (tabs ++ tabs) ++ '\n' : c : newCode
                        '}' ->
                            format (tabSize - tabNumber) tabNumber cs $
                            '\n' : c : spaces (tabSize - tabNumber) ++ newCode
                        ';' ->
                            format tabSize tabNumber cs $
                            tabs ++ '\n' : c : newCode
                        _ -> format tabSize tabNumber cs $ c : newCode

start :: Int -> String -> String
start tabSize code =
    "#include <stdio.h>\n\n#define STACK_SIZE 1000\n\nint main(void) {" ++
    newline ++
    "double stack[STACK_SIZE];" ++
    newline ++
    "for (int i = 0; i < STACK_SIZE; i++) {\n" ++
    tabs ++
    tabs ++
    "stack[i] = 0.0;" ++
    newline ++
    "}" ++ newline ++ formatC tabSize code ++ newline ++ "return 0;\n}\n"
  where
    tabs :: String
    tabs = spaces tabSize
    newline :: String
    newline = "\n" ++ tabs

gen :: [Instruction] -> String -> Int -> Int -> String
gen instructions code tabSize idx =
    case instructions of
        ins:inss ->
            case ins of
                Push d ->
                    gen
                        inss
                        (code ++ "stack[" ++ show idx ++ "] = " ++ show d ++ ";")
                        tabSize $
                    idx + 1
                Pop ->
                    gen inss (code ++ "stack[" ++ show idx ++ "] = 0;") tabSize $
                    idx - 1
                Add ->
                    gen
                        inss
                        (code ++
                         "stack[" ++
                         show
                             (let idxRes = idx - 2
                               in if idxRes < 0
                                      then 0
                                      else idxRes) ++
                         "] = stack[" ++
                         show
                             (let idxRes = idx - 2
                               in if idxRes < 0
                                      then 0
                                      else idxRes) ++
                         "] + stack[" ++
                         show
                             (let idxRes = idx - 1
                               in if idxRes == 0
                                      then 1
                                      else idxRes) ++
                         "];")
                        tabSize
                        idx
                Debug ->
                    error
                        "The debug instruction feature is only available for executing the code, not compiling the code to C code"
                Print ->
                    gen
                        inss
                        (code ++ "printf(\"%lf\\n\", stack[0]);")
                        tabSize
                        idx
        _ -> formatC tabSize code

defaultCompile :: [Instruction] -> String
defaultCompile i = Codegen.start 4 $ Codegen.gen i [] 4 0

-- https://stackoverflow.com/a/5852820/13243613
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal : xs
    | otherwise = x : replaceNth (n - 1) newVal xs

run :: [Double] -> [Instruction] -> IO ()
run stack inss =
    unless
        (null inss)
        case head inss of
            Push d -> run (stack ++ [d]) $ tail inss
            Pop -> run (init stack) $ tail inss
            Add ->
                let elementPosToChange = length stack - 2
                 in let elementPosToAdd = length stack - 1
                     in run (replaceNth
                                 elementPosToChange
                                 (stack !! elementPosToChange +
                                  stack !! elementPosToAdd)
                                 stack) $
                        tail inss
            Debug -> print stack >> run stack (tail inss)
            Print -> print (head stack) >> run stack (tail inss)

defaultRun :: [Instruction] -> IO ()
defaultRun = run []
