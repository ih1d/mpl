module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec
import Text.Parsec.Token

mpl :: TokenParser ()
mpl = makeTokenParser mplDef

mplDef :: LanguageDef ()
mplDef = LanguageDef
    { commentStart   = "(*"
    , commentEnd     = "*)"
    , commentLine    = ""
    , nestedComments = True
    , identStart     = letter <|> char '_'
    , identLetter    = alphaNum <|> oneOf "_'"
    , opStart        = opLetter mplDef
    , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames= ops
    , reservedNames  = names
    , caseSensitive  = True
    }
    where
        ops = ["+", "-", "*", "/", "^", "=", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "|>", "->"]
        names = ["use", "let", "in", "not", "if", "then", "else", "true", "false", "rec", "lambda"]

mplReserved :: String -> Parser ()
mplReserved = reserved mpl

mplReservedOp :: String -> Parser ()
mplReservedOp = reservedOp mpl

mplIdentifier :: Parser String
mplIdentifier = identifier mpl

mplNatural :: Parser Integer
mplNatural = natural mpl

mplInteger :: Parser Integer
mplInteger = integer mpl

mplFloat :: Parser Double
mplFloat = float mpl

mplStringLiteral :: Parser String
mplStringLiteral = stringLiteral mpl

mplCharLiteral :: Parser Char
mplCharLiteral = charLiteral mpl

mplSymbol :: String -> Parser String
mplSymbol = symbol mpl

mplLexeme :: Parser a -> Parser a
mplLexeme = lexeme mpl

mplWhiteSpace :: Parser ()
mplWhiteSpace = whiteSpace mpl

mplParens :: Parser a -> Parser a
mplParens = parens mpl

mplBraces :: Parser a -> Parser a
mplBraces = braces mpl

mplBrackets :: Parser a -> Parser a
mplBrackets = brackets mpl

mplAngles :: Parser a -> Parser a
mplAngles = angles mpl

mplSemi :: Parser String
mplSemi = semi mpl

mplComma :: Parser String
mplComma = comma mpl

mplColon :: Parser String
mplColon = colon mpl

mplDot :: Parser String
mplDot = dot mpl

mplCommaSep :: Parser a -> Parser [a]
mplCommaSep = commaSep mpl

mplCommaSep1 :: Parser a -> Parser [a]
mplCommaSep1 = commaSep1 mpl

mplSemiSep :: Parser a -> Parser [a]
mplSemiSep = semiSep mpl

mplSemiSep1 :: Parser a -> Parser [a]
mplSemiSep1 = semiSep1 mpl