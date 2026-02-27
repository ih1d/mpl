module Syntax where

import Data.Bits (shiftR, (.&.))
import Data.Word (Word64)
import Data.Vector (Vector, (!))
import Data.Text (Text, unpack)

type Id = String

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

data Value
    = IntV Integer
    | DoubleV Double
    | BoolV Bool
    | StringV Text
    | UnitV ()
    | ClosureV [(Id, Value)] [Id] Expr
    | BuiltinV Id
    | DNA (Vector Word64, Int)
    | RNA (Vector Word64, Int)

instance Show Value where
    show (IntV i) = show i
    show (DoubleV d) = show d
    show (BoolV True) = "true"
    show (BoolV False) = "false"
    show (UnitV u) = show u
    show (StringV t) = unpack t
    show (ClosureV {}) = "<closure>"
    show (BuiltinV name) = "<builtin:" ++ name ++ ">"
    show (DNA (ws, len)) = showSeq dnaChar ws len
    show (RNA (ws, len)) = showSeq rnaChar ws len

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

showSeq :: (Word64 -> Char) -> Vector Word64 -> Int -> String
showSeq decode v = go 0
  where
    go _ 0 = []
    go idx remaining =
        let w = v ! idx
            n = min 32 remaining
        in [decode ((w `shiftR` (2 * (n - 1 - i))) .&. 3) | i <- [0..n-1]] ++ go (idx + 1) (remaining - n)

dnaChar :: Word64 -> Char
dnaChar 0 = 'A'
dnaChar 1 = 'C'
dnaChar 2 = 'G'
dnaChar _ = 'T'

rnaChar :: Word64 -> Char
rnaChar 0 = 'A'
rnaChar 1 = 'C'
rnaChar 2 = 'G'
rnaChar _ = 'U'
