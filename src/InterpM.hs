{-# LANGUAGE TemplateHaskell #-}

module InterpM where

import Control.Monad.Except
import Control.Monad.State
import Lens.Micro.Platform
import Syntax

data Env = Env
    { _variables :: [(Id, Expr)]
    , _types :: [(Id, Types)]
    , _plans :: [(Int, Plan)]
    , _nextRef :: Int
    }
makeLenses ''Env

newtype InterpM a = M {unM :: ExceptT Error (StateT Env IO) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadIO, MonadError Error)

runM :: InterpM a -> Env -> IO (Either Error a)
runM (M m) = evalStateT (runExceptT m)

runMState :: InterpM a -> Env -> IO (Either Error a, Env)
runMState (M m) = runStateT (runExceptT m)

lookupVar :: Id -> InterpM Expr
lookupVar var = do
    varEnv <- use variables
    case lookup var varEnv of
        Nothing -> throwError (Unbound var)
        Just expr -> pure expr

lookupType :: Id -> InterpM Types
lookupType t = do
    typeEnv <- use types
    case lookup t typeEnv of
        Nothing -> throwError (Unbound t)
        Just t' -> pure t'

bindVar :: Id -> Expr -> InterpM ()
bindVar var expr = variables %= ((var, expr) :)

bindType :: Id -> Types -> InterpM ()
bindType tname ty = types %= ((tname, ty) :)

bindPlan :: Plan -> InterpM ()
bindPlan p = do
    ref <- use nextRef
    plans %= ((ref, p) :)
    updateNextRef

updateNextRef :: InterpM ()
updateNextRef = nextRef += 1

io :: IO a -> InterpM a
io = liftIO
