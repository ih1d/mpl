module Main where

import Eval
import Parser (parseLine)
import Syntax
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> runFile f
        [] -> hSetBuffering stdout NoBuffering >> repl initialEnv
        _ -> putStrLn "Usage: mplc [FILE]"

runFile :: FilePath -> IO ()
runFile f = do
    src <- readFile f
    let lns = lines src
    go initialEnv lns
  where
    go _ [] = pure ()
    go env (l : ls) = case parseLine l of
        Left err -> print err
        Right Nothing -> go env ls
        Right (Just expr) -> do
            (mval, env') <- runEvalExpr env expr
            case mval of
                Left err -> print err
                Right _ -> pure ()
            go env' ls

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
