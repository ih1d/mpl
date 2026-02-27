module Main where
    
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Parser
import Eval

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl

repl :: IO ()
repl = do
    putStr "MPL> "
    l <- getLine
    case parser l of
        Left err -> print err
        Right expr -> do
            mval <- runM (tc expr >> eval expr) 
            case mval of
                Left err' -> print err'
                Right val -> print val
    repl