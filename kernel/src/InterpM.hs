module InterpM where

import Control.Monad.Except
import Control.Monad.State
import Syntax

newtype Env = Env { variables :: [(Id, Expr)] }

newtype InterpM a = M {unM :: ExceptT Error (StateT Env IO) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadIO, MonadError Error)

runM :: InterpM a -> Env -> IO (Either Error a)
runM (M m) = evalStateT (runExceptT m)

runMState :: InterpM a -> Env -> IO (Either Error a, Env)
runMState (M m) = runStateT (runExceptT m)

lookupVar :: Id -> InterpM Expr
lookupVar var = do
    env <- gets variables
    case lookup var env of
        Nothing -> throwError (Unbound var)
        Just expr -> pure expr

bindVar :: Id -> Expr -> InterpM ()
bindVar var expr = do
    env <- gets variables
    case lookup var env of
        Nothing -> put $ Env ((var, expr) : env)
        Just _ -> let env' = [(x,e) | (x,e) <- env, x /= var] in put $ Env ((var,expr) : env')

io :: IO a -> InterpM a
io = liftIO
