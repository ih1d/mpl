module Syntax where

import Data.Complex (Complex)
import Data.List (intercalate)
import MPLTypes
import Text.Parsec (ParseError)

type Id = String

data Types
    = NumT
    | IntT
    | DoubleT
    | RatioT
    | ComplexT
    | BoolT
    | StringT
    | FunT
    | DNAT
    | RNAT
    | UnitT
    | TupleT [Types]
    | DataframeT
    deriving (Eq)

instance Show Types where
    show IntT = "int"
    show RatioT = "rational"
    show ComplexT = "complex"
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

data Value
    = IntV Integer
    | RatioV Rational
    | ComplexV (Complex Value)
    | DoubleV Double
    | BoolV Bool
    | StringV String
    | TupleV [Value]
    | UnitV ()
    | ClosureV [Id] Expr
    | DNAV DNA
    | RNAV RNA
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
typeOf (RatioV _) = RatioT
typeOf (ComplexV _) = ComplexT

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
    show (RatioV r) = show r
    show (ComplexV c) = show c

data Op
    = Add
    | Sub
    | Mul
    | Div
    | Pow
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
    show Eq = "=="
    show NotEq = "!="
    show Gt = ">"
    show GtEq = ">="
    show Lt = "<"
    show LtEq = "<="
    show Pipe = "|>"

data Expr
    = Const Value
    | BinOp Op Expr Expr
    | Var Id
    | Tuple [Expr]
    | Read String
    | Write String
    deriving (Eq)

instance Show Expr where
    show (Const v) = show v
    show (BinOp op e0 e1) = show e0 ++ " " ++ show op ++ " " ++ show e1
    show (Var v) = v
    show (Tuple es) = "(" ++ intercalate ", " (map show es) ++ ")"
    show (Read str) = "read " ++ str
    show (Write str) = "write " ++ str

data Error
    = ParseE ParseError
    | NotInScope Id Expr
    | RuntimeError String
    | TypeError Types Types
    | Unbound Id
    | ParityMismatch Id Int
instance Show Error where
    show (ParseE pe) = show pe
    show (TypeError t0 t1) = "expected type: " ++ show t0 ++ ", got: " ++ show t1
    show (Unbound v) = "unbound name: " ++ v
    show (RuntimeError msg) = msg
    show (NotInScope f e) = f ++ " is not in scope in the expression: " ++ show e
    show (ParityMismatch f n) = "parity mismatch for: " ++ f ++ ", expects " ++ show n ++ " number of argument(s)"
