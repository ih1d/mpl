module Main where

import Eval
import Syntax
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> readFile f >>= runFile
        [] -> hSetBuffering stdout NoBuffering >> repl initEnv
        _ -> putStrLn "Usage: mplc [FILE]"

runFile :: FilePath -> IO ()
runFile f = loop initEnv (lines f)
    where
        loop _ [] = pure ()
        loop env [c] = do
            (mval, _) <- runEval env c
            case mval of
                Left err -> print err
                Right (v, t) -> putStrLn (show v ++ " : " ++ show t)
        loop env (c:cs) = do
            (mval, env') <- runEval env c
            case mval of
                Left err -> print err
                Right _ -> loop env' cs

repl :: Env -> IO ()
repl env = do
    putStr "MPL> "
    l <- getLine
    if null l
        then repl env
        else do
            (mval, env') <- runEval env l
            case mval of
                Left err -> print err
                Right (v, t) -> putStrLn (show v ++ " : " ++ show t)
            repl env'
