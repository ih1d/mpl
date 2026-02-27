module MPL where

import Syntax
import Control.Monad.State
import Control.Monad.Except
import Text.Parsec (ParseError)

data Error
    = ParseE ParseError
    | RuntimeError String
    | TypeError Types Types
    | Unbound Id
instance Show Error where
    show (ParseE pe) = show pe
    show (TypeError t0 t1) = "expected type: " ++ show t0 ++ ", got: " ++ show t1
    show (Unbound v) = "unbound name: " ++ v
    show (RuntimeError msg) = msg

newtype M a = M { unM :: ExceptT Error (StateT Env IO) a }
    deriving (Functor, Applicative, Monad, MonadState Env, MonadIO, MonadError Error)

runM :: M a -> Env -> IO (Either Error a)
runM (M m) = evalStateT (runExceptT m)

getEnv :: M Env
getEnv = get

withEnv :: Env -> M a -> M a
withEnv env action = do
    old <- get
    put env
    result <- action
    put old
    pure result

lookupVar :: Id -> M Expr
lookupVar var = do
    env <- getEnv
    case lookup var env of
        Nothing -> throwError (Unbound var)
        Just expr -> pure expr

bindVar :: Id -> Expr -> M ()
bindVar var expr = do
    env <- getEnv
    put ((var, expr): env)

io :: IO a -> M a
io = liftIO