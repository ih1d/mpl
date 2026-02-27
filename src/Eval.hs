module Eval (eval, tc, runM) where

import Syntax
import Control.Monad.State
import Control.Monad.Except
import Text.Parsec (ParseError)
import Control.Monad (void)
import Prelude hiding (print)

type Env = [(Id, Value)]

io :: IO a -> M a
io = liftIO

builtIns :: [(Id, Expr -> M Value)]
builtIns =
    [("print", print)]

print :: Expr -> M Value
print e = do
    v <- eval e
    io $ putStr (show v)
    pure $ UnitV ()

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

type M a = ExceptT Error (StateT Env IO) a

runM :: M a -> IO (Either Error a)
runM m = evalStateT (runExceptT m) []

lookupVar :: Id -> M Value
lookupVar var = do
    env <- get
    case lookup var env of
        Nothing -> throwError (Unbound var)
        Just val -> pure val

bindVar :: Id -> Value -> M ()
bindVar var val = get >>= \env -> put ((var, val): env)

-- type checker
tc :: Expr -> M Types
tc (Const (IntV _)) = pure IntT
tc (Const (DoubleV _)) = pure DoubleT
tc (Const (BoolV _)) = pure BoolT
tc (Const (StringV _)) = pure StringT
tc (Const (FunV _)) = pure FunT
tc (Const (DNA _)) = pure DNAT
tc (Const (RNA _)) = pure RNAT
tc (UnOp Not (Const (BoolV _))) = pure BoolT
tc (UnOp Not e) = do
    t <- tc e
    throwError $ TypeError t t
tc (UnOp op _) = throwError $ RuntimeError ("operator: " ++ show op ++ " is not unary")
tc (BinOp op e0 e1) =
    case op of
        Add -> undefined
        Sub -> undefined
        Mul -> undefined
        Div -> undefined
        Pow -> undefined
        And -> undefined
        Or -> undefined
        Not -> undefined
        Eq -> undefined
        NotEq -> undefined
        Gt -> undefined
        GtEq -> undefined
        Lt -> undefined
        LtEq -> undefined
        Pipe -> undefined        
tc (If cnd e0 e1) = do
    tcnd <- tc cnd
    case tcnd of
        BoolT -> do
            te0 <- tc e0
            te1 <- tc e1
            if te0 == te1 then pure te0 else throwError $ TypeError te0 te1
        _ -> throwError $ TypeError BoolT tcnd
tc (Var v) = lookupVar v >>= tc . Const
tc (Let _ e0 e1) = do
    void $ tc e0
    tc e1
tc (LetF _ _ e) = tc e
tc (LetR _ _ e) = tc e
tc (Lam _ _) = pure FunT
tc (App f _) = tc f

-- evaluator
eval :: Expr -> M Value
eval (Const i) = pure i
eval (UnOp Not (Const (BoolV b))) = pure $ BoolV (not b)
eval (UnOp op _) = throwError $ RuntimeError ("not unary operator: " ++ show op)
eval (BinOp op e0 e1) =
    case op of
        Add -> undefined
        Sub -> undefined
        Mul -> undefined
        Div -> undefined
        Pow -> undefined
        And -> undefined
        Or -> undefined
        Not -> undefined
        Eq -> undefined
        NotEq -> undefined
        Gt -> undefined
        GtEq -> undefined
        Lt -> undefined
        LtEq -> undefined
        Pipe -> undefined

eval (If cnd e0 e1) = do
    cnd' <- eval cnd
    case cnd' of
        BoolV True -> eval e0
        BoolV False -> eval e1
        _ -> do
            t <- tc cnd
            throwError $ RuntimeError ("if expects bool, but got: " ++ show t)
eval (Var v) = lookupVar v
eval (Let v e0 e1) = do
    val <- eval e0
    bindVar v val
    eval e1
eval (LetF{}) = undefined
eval (LetR{}) = undefined
eval (Lam{}) = undefined
eval (App f args) = do
    f' <- eval f
    case f' of
        FunV f'' -> pure $ f'' args
        _ -> undefined