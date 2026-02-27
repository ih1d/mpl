module Main where
    
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Parser
import Eval (tc, eval, initEnv, runM)

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl

repl :: IO ()
repl = do
    putStr "MPL> "
    l <- getLine
    case parser l of
        Left err -> print err
        Right expr -> do
            mval <- runM (tc expr >> eval expr) initEnv
            case mval of
                Left err' -> print err'
                Right val -> print val
    repl