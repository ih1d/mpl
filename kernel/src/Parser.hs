module Parser (parser, parseLine) where

import Data.Functor.Identity (Identity)
import Lexer
import MPLTypes
import Syntax
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

binary :: String -> Op -> Assoc -> Operator String () Identity Expr
binary s op = Infix (mplReservedOp s >> return (BinOp op))

opTable :: OperatorTable String () Identity Expr
opTable =
    [ [binary "^" Pow AssocRight]
    , [binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
    , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
    ,
        [ binary "<" Lt AssocNone
        , binary ">" Gt AssocNone
        , binary "<=" LtEq AssocNone
        , binary ">=" GtEq AssocNone
        ]
    , [binary "==" Eq AssocNone, binary "!=" NotEq AssocNone]
    , [binary "|>" Pipe AssocLeft]
    ]

parseInt :: Parser Expr
parseInt = Const . IntV <$> mplNatural

parseDouble :: Parser Expr
parseDouble = Const . DoubleV <$> mplFloat

parseBool :: Parser Expr
parseBool = Const . BoolV <$> (True <$ mplReserved "true" <|> False <$ mplReserved "false")

parseStr :: Parser Expr
parseStr = Const . StringV <$> mplStringLiteral

parseVar :: Parser Expr
parseVar = Var <$> mplIdentifier

parseDNA :: Parser Expr
parseDNA = try $ mplLexeme $ do
    dna <- many1 (char 'A' <|> char 'C' <|> char 'G' <|> char 'T')
    notFollowedBy (oneOf "ACGU")
    return $ Const (DNAV (DNA dna))

parseRNA :: Parser Expr
parseRNA = try $ mplLexeme $ do
    rna <- many1 (char 'A' <|> char 'C' <|> char 'G' <|> char 'U')
    notFollowedBy (oneOf "ACGT")
    return $ Const (RNAV (RNA rna))

parseTupleOrParens :: Parser Expr
parseTupleOrParens = do
    _ <- mplSymbol "("
    es <- mplCommaSep parseTerm
    _ <- mplSymbol ")"
    pure $ case es of
        [] -> Const (UnitV ())
        [e] -> e
        _ -> Tuple es

parseRead :: Parser Expr
parseRead = do
    mplReserved "read"
    Read <$> mplStringLiteral

parseWrite :: Parser Expr
parseWrite = do
    mplReserved "write"
    Write <$> mplStringLiteral

parseAtom :: Parser Expr
parseAtom =
    parseTupleOrParens
        <|> try parseDouble
        <|> parseInt
        <|> parseBool
        <|> parseStr
        <|> parseDNA
        <|> parseRNA
        <|> parseRead
        <|> parseWrite
        <|> parseVar

parseTerm :: Parser Expr
parseTerm = buildExpressionParser opTable parseAtom

parser :: String -> Either ParseError Expr
parser = parse (mplWhiteSpace *> parseTerm <* eof) "mpl"

parseLine :: String -> Either ParseError (Maybe Expr)
parseLine = parse (mplWhiteSpace *> optionMaybe parseTerm <* eof) "mpl"
