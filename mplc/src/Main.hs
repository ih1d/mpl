module Main where

import Eval
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> do
            contents <- readFile f
            (mval, _) <- runEval initEnv contents
            case mval of
                Left err -> print err
                Right val -> print val
        _ -> putStrLn "Usage: mplc FILE"
