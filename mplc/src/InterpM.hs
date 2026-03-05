module InterpM where

import Control.Monad.Except
import Control.Monad.State
import Syntax

newtype InterpM a = M {unM :: ExceptT Error (StateT Env IO) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadIO, MonadError Error)

runM :: InterpM a -> Env -> IO (Either Error a)
runM (M m) = evalStateT (runExceptT m)

runMState :: InterpM a -> Env -> IO (Either Error a, Env)
runMState (M m) = runStateT (runExceptT m)

getEnv :: InterpM Env
getEnv = get

withEnv :: Env -> InterpM a -> InterpM a
withEnv env action = do
    old <- get
    put env
    result <- action
    put old
    pure result

lookupVar :: Id -> InterpM Expr
lookupVar var = do
    env <- getEnv
    case lookup var env of
        Nothing -> throwError (Unbound var)
        Just expr -> pure expr

bindVar :: Id -> Expr -> InterpM ()
bindVar var expr = do
    env <- getEnv
    put ((var, expr) : env)

io :: IO a -> InterpM a
io = liftIO
