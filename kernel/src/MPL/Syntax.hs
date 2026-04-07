module MPL.Syntax where

import Text.Parsec (ParseError)
import Prelude hiding (Read)

type Id = String

type Program = [Stmt]

data Read
    = Read10X
    | Read10XH5
    deriving (Eq)

instance Show Read where
    show Read10X = "read10x"
    show Read10XH5 = "read10x_h5"

data Expr
    = VarE Id
    | ReadE Read String
    deriving (Eq)

instance Show Expr where
    show (VarE v) = v
    show (ReadE r str) = show r ++ " " ++ str

data Stmt
    = Assign Id Expr
    | ExprS Expr
    deriving (Eq)

instance Show Stmt where
    show (Assign v e) = v ++ " = " ++ show e
    show (ExprS e) = show e

data Error
    = ParseE ParseError
    | RuntimeError String
    | Unbound Id
instance Show Error where
    show (ParseE pe) = show pe
    show (Unbound v) = "unbound name: " ++ v
    show (RuntimeError msg) = msg
