module Syntax where

import BioPrims

type Id = String

type Env = [(Id, Expr)]

data Types
    = IntT
    | BoolT
    | DoubleT
    | StringT
    | NumT
    | FunT
    | DNAT
    | RNAT
    | UnitT
    | PrimitiveT
    deriving (Eq)

instance Show Types where
    show IntT = "int"
    show BoolT = "bool"
    show StringT = "string"
    show FunT = "<function>"
    show DoubleT = "double"
    show DNAT = "DNA"
    show RNAT = "RNA"
    show NumT = "numerical"
    show UnitT = "()"
    show PrimitiveT = "<primitive>"

data Value
    = IntV Integer
    | DoubleV Double
    | BoolV Bool
    | StringV String
    | UnitV ()
    | ClosureV [Id] Env Expr
    | DNAV DNA
    | RNAV RNA

instance Show Value where
    show (IntV i) = show i
    show (DoubleV d) = show d
    show (BoolV True) = "true"
    show (BoolV False) = "false"
    show (UnitV u) = show u
    show (StringV t) = t
    show (ClosureV {}) = "<closure>"
    show (DNAV dna)  = show dna
    show (RNAV rna) = show rna

data Op
    = Add
    | Sub
    | Mul
    | Div
    | Pow
    | And
    | Or
    | Not
    | Eq
    | NotEq
    | Gt
    | GtEq
    | Lt
    | LtEq
    | Pipe

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"
    show And = "&&"
    show Or = "||"
    show Not = "not"
    show Eq = "=="
    show NotEq = "!="
    show Gt = ">"
    show GtEq = ">="
    show Lt = "<"
    show LtEq = "<="
    show Pipe = "|>"

data Expr
    = Const Value
    | UnOp Op Expr
    | BinOp Op Expr Expr
    | Primitive Id
    | If Expr Expr Expr
    | Var Id
    | Let Id Expr Expr
    | LetF Id [Id] Expr
    | LetR Id [Id] Expr
    | Lam [Id] Expr
    | App Expr Expr

instance Show Expr where
    show (Const v) = show v
    show (UnOp o e) = show o ++ " " ++ show e
    show (BinOp op e0 e1) = show e0 ++ " " ++ show op ++ " " ++ show e1
    show (If cnd e0 e1) = "if " ++ show cnd ++ " then " ++ show e0 ++ " else " ++ show e1
    show (Var v) = v
    show (Let v e0 e1) = "let " ++ v ++ " = " ++ show e0 ++ " in " ++ show e1
    show (LetF f args e) = "let " ++ f ++ " " ++ unwords args ++ " = " ++ show e
    show (LetR f args e) = "let rec " ++ f ++ " " ++ unwords args ++ " = " ++ show e
    show (Lam args e) = "lambda " ++ unwords args ++ " -> " ++ show e
    show (App e0 e1) = show e0 ++ " " ++ show e1
    show (Primitive p) = p