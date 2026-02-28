module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Eval

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl initEnv

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
