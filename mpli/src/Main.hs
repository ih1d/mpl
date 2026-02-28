module Main where
    
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Eval 

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl

repl :: IO ()
repl = do
    putStr "MPL> "
    l <- getLine
    if null l
        then repl
        else do
            mval <- runEval l
            case mval of
                Left err -> print err
                Right (v, t)-> putStrLn (show v ++ " : " ++ show t)
            repl