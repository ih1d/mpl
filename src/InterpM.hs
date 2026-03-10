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

getEnv :: InterpM [(Id, Expr)]
getEnv = gets variables

getTypeEnv :: InterpM [(Id, Types)]
getTypeEnv = gets types

getBackend :: InterpM Backend
getBackend = gets backend

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

lookupType :: Id -> InterpM Types
lookupType t = do
    typeEnv <- getTypeEnv
    case lookup t typeEnv of
        Nothing -> throwError (Unbound t)
        Just t' -> pure t'

bindVar :: Id -> Expr -> InterpM ()
bindVar var expr = do
    env <- getEnv
    Env ((var, expr) : env) <$> getTypeEnv <*> getBackend >>= put

bindType :: Id -> Types -> InterpM ()
bindType tname ty = do
    tyEnv <- getTypeEnv
    Env <$> getEnv <*> pure ((tname, ty) : tyEnv) <*> getBackend >>= put

setBackend :: Backend -> InterpM ()
setBackend b = Env <$> getEnv <*> getTypeEnv <*> pure b >>= put

io :: IO a -> InterpM a
io = liftIO
