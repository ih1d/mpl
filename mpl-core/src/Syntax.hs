module Syntax where

import Data.List (intercalate)
import MPLTypes
import System.Arrow (Table)
import Text.Parsec (ParseError)

type Id = String

data Env = Env
    { variables :: [(Id, Expr)]
    , types :: [(Id, Types)]
    }

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
    | TupleT [Types]
    | DataframeT
    | ADTT Id
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
    show (TupleT types) = "(" ++ intercalate ", " (map show types) ++ ")"
    show DataframeT = "dataframe"
    show (ADTT t) = t

data Value
    = IntV Integer
    | DoubleV Double
    | BoolV Bool
    | StringV String
    | TupleV [Value]
    | UnitV ()
    | ClosureV [Id] Expr
    | DNAV DNA
    | RNAV RNA
    | DataframeV Table
    | ConstrV Id Id [Value]
    | ConstrFunV Id Id Int [Value]
    deriving (Eq)

typeOf :: Value -> Types
typeOf (IntV _) = IntT
typeOf (DoubleV _) = DoubleT
typeOf (BoolV _) = BoolT
typeOf (StringV _) = StringT
typeOf (UnitV _) = UnitT
typeOf (ClosureV{}) = FunT
typeOf (DNAV _) = DNAT
typeOf (RNAV _) = RNAT
typeOf (TupleV vals) = TupleT (map typeOf vals)
typeOf (DataframeV _) = DataframeT
typeOf (ConstrV p _ _) = ADTT p
typeOf (ConstrFunV p _ _ _) = ADTT p

instance Show Value where
    show (IntV i) = show i
    show (DoubleV d) = show d
    show (BoolV True) = "true"
    show (BoolV False) = "false"
    show (UnitV u) = show u
    show (StringV t) = t
    show (ClosureV{}) = "<closure>"
    show (DNAV dna) = show dna
    show (RNAV rna) = show rna
    show (TupleV vals) = "(" ++ intercalate ", " (map show vals) ++ ")"
    show (DataframeV table) = show table
    show (ConstrV parent constr vals) = parent ++ " : " ++ constr ++ " " ++ unwords (map show vals)
    show (ConstrFunV parent constr a vals) = parent ++ " : " ++ constr ++ " " ++ unwords (map show vals) ++ " " ++ unwords (replicate a "*")

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
    deriving (Eq)

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

data Constructor = Constructor
    { parent :: Id
    , constrName :: Id
    , arity :: Int
    , tag :: Int
    }
    deriving (Eq)

instance Show Constructor where
    show (Constructor p n a _) = p ++ " : " ++ n ++ " " ++ unwords (replicate a "*")

data TypeInfo = TypeInfo
    { typeName :: Id
    , typeVars :: [Id]
    , constr :: [Constructor]
    }
    deriving (Eq)
instance Show TypeInfo where
    show (TypeInfo n vars c) = n ++ " " ++ unwords vars ++ unwords (map show c)

data Expr
    = Const Value
    | UnOp Op Expr
    | BinOp Op Expr Expr
    | If Expr Expr Expr
    | Var Id
    | Let Id Expr
    | LetI Id Expr Expr
    | LetF Id [Id] Expr
    | LetR Id [Id] Expr
    | Lam [Id] Expr
    | App Expr [Expr]
    | Tuple [Expr]
    | Type TypeInfo
    deriving (Eq)

instance Show Expr where
    show (Const v) = show v
    show (UnOp o e) = show o ++ " " ++ show e
    show (BinOp op e0 e1) = show e0 ++ " " ++ show op ++ " " ++ show e1
    show (If cnd e0 e1) = "if " ++ show cnd ++ " then " ++ show e0 ++ " else " ++ show e1
    show (Var v) = v
    show (Let v e) = "let " ++ v ++ " = " ++ show e
    show (LetI v e0 e1) = "let " ++ v ++ " = " ++ show e0 ++ " in " ++ show e1
    show (LetF f args e) = "let " ++ f ++ " " ++ unwords args ++ " = " ++ show e
    show (LetR f args e) = "let rec " ++ f ++ " " ++ unwords args ++ " = " ++ show e
    show (Lam args e) = "lambda " ++ unwords args ++ " -> " ++ show e
    show (App e0 e1) = show e0 ++ " " ++ show e1
    show (Tuple es) = "(" ++ intercalate ", " (map show es) ++ ")"
    show (Type t) = show t

data Error
    = ParseE ParseError
    | NotInScope Id Expr
    | RuntimeError String
    | TypeError Types Types
    | Unbound Id
instance Show Error where
    show (ParseE pe) = show pe
    show (TypeError t0 t1) = "expected type: " ++ show t0 ++ ", got: " ++ show t1
    show (Unbound v) = "unbound name: " ++ v
    show (RuntimeError msg) = msg
    show (NotInScope f e) = f ++ " is not in scope in the expression: " ++ show e
