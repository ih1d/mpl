module MPL.Parser (parser, parseLine) where

import MPL.Lexer
import MPL.Syntax
import Text.Parsec
import Text.Parsec.String (Parser)

parseRead :: Parser Expr
parseRead = do
    r <- (Read10XH5 <$ mplReserved "read10x_h5") <|> (Read10X <$ mplReserved "read10x")
    ReadE r <$> mplStringLiteral

parseVar :: Parser Expr
parseVar = VarE <$> mplIdentifier

parseExpr :: Parser Expr
parseExpr = parseRead <|> parseVar

parseAssign :: Parser Stmt
parseAssign = do
    v <- mplIdentifier
    mplReservedOp "="
    Assign v <$> parseExpr

parseProg :: Parser Program
parseProg = many (try parseAssign <|> (ExprS <$> parseExpr))

parser :: String -> Either ParseError Program
parser = parse (mplWhiteSpace *> parseProg <* eof) "mpl"

parseLine :: String -> Either ParseError (Maybe Program)
parseLine = parse (mplWhiteSpace *> optionMaybe parseProg <* eof) "mpl"
