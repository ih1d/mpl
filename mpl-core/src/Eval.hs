module Eval (tc, eval, initEnv, runM) where

import MPL
import Control.Monad.Except (throwError)
import Syntax
import Control.Monad (void)

initEnv :: Env
initEnv = [("print", BuiltinV "print")]

applyBuiltin :: Id -> Value -> M Value
applyBuiltin "print" v = do
    io $ print v
    pure $ UnitV ()
    
applyBuiltin name _ = throwError $ RuntimeError ("unknown builtin: " ++ name)

-- type checker
tc :: Expr -> M Types
tc (Const (IntV _)) = pure IntT
tc (Const (DoubleV _)) = pure DoubleT
tc (Const (BoolV _)) = pure BoolT
tc (Const (StringV _)) = pure StringT
tc (Const (ClosureV {})) = pure FunT
tc (Const (BuiltinV _)) = pure FunT
tc (Const (DNA _)) = pure DNAT
tc (Const (RNA _)) = pure RNAT
tc (UnOp Not (Const (BoolV _))) = pure BoolT
tc (UnOp Not e) = do
    t <- tc e
    throwError $ TypeError BoolT t
tc (UnOp op _) = throwError $ RuntimeError ("operator: " ++ show op ++ " is not unary")
tc (BinOp op e0 e1) =
    case op of
        Add -> do
            t0 <- tc e0
            t1 <- tc e1
            if t0 == t1 then pure t0 else throwError $ TypeError t0 t1
        Sub -> do
            t0 <- tc e0
            t1 <- tc e1
            if t0 == t1 then pure t0 else throwError $ TypeError t0 t1
        Mul -> do
            t0 <- tc e0
            t1 <- tc e1
            if t0 == t1 then pure t0 else throwError $ TypeError t0 t1
        Div -> do
            t0 <- tc e0
            t1 <- tc e1
            if t0 == t1 then pure t0 else throwError $ TypeError t0 t1
        Pow -> do
            t0 <- tc e0
            if t0 == IntT || t0 == DoubleT then pure t0 else throwError $ TypeError t0 IntT
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
eval (LetF name params body) = do
    env <- getEnv
    let closure = ClosureV env params body
    bindVar name closure
    pure closure
eval (LetR name params body) = do
    env <- getEnv
    let closure = ClosureV ((name, closure) : env) params body
    bindVar name closure
    pure closure
eval (Lam params body) = do
    env <- getEnv
    pure $ ClosureV env params body
eval (App f arg) = do
    f' <- eval f
    arg' <- eval arg
    case f' of
        ClosureV cenv (p:ps) body -> do
            let newEnv = (p, arg') : cenv
            if null ps
                then withEnv newEnv (eval body)
                else pure $ ClosureV newEnv ps body
        ClosureV _ [] _ -> throwError $ RuntimeError "too many arguments applied"
        BuiltinV name -> applyBuiltin name arg'
        _ -> throwError $ RuntimeError "application of non-function"