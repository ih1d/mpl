module Main where

import Parser
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "El MegaProbe Language, version 0.1, type :h for help"
    repl

repl :: IO ()
repl = do
    putStr "MPL> "
    l <- getLine
    case parseLine l of
        Left err -> print err >> repl
        Right expr -> print expr >> repl
