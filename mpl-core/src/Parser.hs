module Parser (parser, parseLine) where

import Data.Char (isLower)
import Data.Functor.Identity (Identity)
import Lexer
import MPLTypes
import Syntax
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

binary :: String -> Op -> Assoc -> Operator String () Identity Expr
binary s op = Infix (mplReservedOp s >> return (BinOp op))

prefix :: String -> Op -> Operator String () Identity Expr
prefix s op = Prefix (mplReservedOp s >> return (UnOp op))

prefixK :: String -> Op -> Operator String () Identity Expr
prefixK s op = Prefix (mplReserved s >> return (UnOp op))

opTable :: OperatorTable String () Identity Expr
opTable =
    [ [prefix "-" Sub, prefixK "not" Not]
    , [binary "^" Pow AssocRight]
    , [binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
    , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
    ,
        [ binary "<" Lt AssocNone
        , binary ">" Gt AssocNone
        , binary "<=" LtEq AssocNone
        , binary ">=" GtEq AssocNone
        ]
    , [binary "==" Eq AssocNone, binary "!=" NotEq AssocNone]
    , [binary "&&" And AssocLeft]
    , [binary "||" Or AssocLeft]
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
    es <- mplCommaSep parseExpr
    _ <- mplSymbol ")"
    pure $ case es of
        [] -> Const (UnitV ())
        [e] -> e
        _ -> Tuple es

parseAtom :: Parser Expr
parseAtom = parseTupleOrParens <|> try parseDouble <|> parseInt <|> parseBool <|> parseStr <|> parseDNA <|> parseRNA <|> parseVar

parseApp :: Parser Expr
parseApp = do
    f <- parseAtom
    args <- many parseAtom
    pure $ case args of
        [] -> f
        _ -> App f args

parseTerm :: Parser Expr
parseTerm = buildExpressionParser opTable parseApp

parseIf :: Parser Expr
parseIf = do
    mplReserved "if"
    cnd <- parseExpr
    mplReserved "then"
    e0 <- parseExpr
    mplReserved "else"
    If cnd e0 <$> parseExpr

parseLet :: Parser Expr
parseLet = do
    mplReserved "let"
    v <- mplIdentifier
    mplReservedOp "="
    Let v <$> parseExpr

parseLetIn :: Parser Expr
parseLetIn = do
    mplReserved "let"
    v <- mplIdentifier
    mplReservedOp "="
    e0 <- parseExpr
    mplReserved "in"
    LetI v e0 <$> parseExpr

parseLetF :: Parser Expr
parseLetF = do
    mplReserved "let"
    f <- mplIdentifier
    args <- many1 mplIdentifier
    mplReservedOp "="
    LetF f args <$> parseExpr

parseLetR :: Parser Expr
parseLetR = do
    mplReserved "let"
    mplReserved "rec"
    f <- mplIdentifier
    args <- many1 mplIdentifier
    mplReservedOp "="
    LetR f args <$> parseExpr

parseLam :: Parser Expr
parseLam = do
    mplReserved "lambda"
    args <- many mplIdentifier
    mplReservedOp "->"
    Lam args <$> parseExpr

parseExpr :: Parser Expr
parseExpr = try parseLetR <|> try parseLetIn <|> try parseLetF <|> parseLet <|> parseLam <|> parseIf <|> parseTerm

parseType :: Parser TypeInfo
parseType = do
    mplReserved "type"
    tname <- mplIdentifier
    params <- many (try paramVar)
    mplReservedOp "="
    frst <- conDecl tname
    rst <- mplBarSep (try (conDecl tname))
    let tagged = zipWith ($) (frst : rst) [0 ..]
    pure $ TypeInfo tname params tagged
  where
    paramVar :: Parser Id
    paramVar = do
        n <- mplIdentifier
        if isLower' n
            then pure n
            else unexpected "type variables are supposed to be lowercase."
    isLower' :: String -> Bool
    isLower' = all isLower
    conDecl :: Id -> Parser (Int -> Constructor)
    conDecl p = do
        n <- mplIdentifier
        a <- length <$> many (try mplIdentifier)
        pure (\x -> Constructor n a x p)

parseProg :: Parser Prog
parseProg = do
    exprs <- try (many parseExpr)
    Prog exprs <$> try (many parseType)

parser :: String -> Either ParseError Prog
parser = parse (mplWhiteSpace *> parseProg <* eof) "mpl"

parseLine :: String -> Either ParseError (Maybe Prog)
parseLine = parse (mplWhiteSpace *> optionMaybe parseProg <* eof) "mpl"
