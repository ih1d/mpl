module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Parser

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