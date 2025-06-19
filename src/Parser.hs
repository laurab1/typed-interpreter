module Parser where

import UntypedExpr
import Type
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

-- Space consumer
sc :: Parser ()
sc = L.space space1 empty empty

-- Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol parser
symbol :: String -> Parser String
symbol = L.symbol sc

-- Reserved keywords to avoid matching as variables
reservedWords :: [String]
reservedWords = ["if", "then", "else", "let", "in", "fun", "true", "false", "letrec", "iszero"]

-- Integer parser
integer :: Parser Int
integer = lexeme L.decimal

-- Boolean parser
boolean :: Parser Bool
boolean = (True <$ symbol "true") <|> (False <$ symbol "false")

-- Identifier (variable) parser
parseIde :: Parser String
parseIde = lexeme $ try $ do
    name <- (:) <$> letterChar <*> many alphaNumChar
    if name `elem` reservedWords
        then fail $ "reserved word " ++ show name
        else return name

-- Type parser
typ :: Parser Type
typ = makeExprParser typTerm [[InfixR (TFun <$ symbol "=>")]]

typTerm :: Parser Type
typTerm = (TInt <$ symbol "int")
      <|> (TBool <$ symbol "bool")
      <|> parens typ

-- Integer expression
parseInt :: Parser UntypedExpr
parseInt = EInt <$> integer

-- Bool expression
parseBool :: Parser UntypedExpr
parseBool = EBool <$> boolean

-- Variable expression
parseVar :: Parser UntypedExpr
parseVar = EVar <$> parseIde

-- Parenthesized expressions
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- IF expression
parseIf :: Parser UntypedExpr
parseIf = do
  _ <- symbol "if"
  cond <- expr
  _ <- symbol "then"
  thn <- expr
  _ <- symbol "else"
  els <- expr
  return (EIf cond thn els)

-- LET expression
parseLet :: Parser UntypedExpr
parseLet = do
  _ <- symbol "let"
  name <- parseIde
  _ <- symbol "="
  val <- expr
  _ <- symbol "in"
  body <- expr
  return (ELet name val body)

-- LET REC expression
parseLetRec :: Parser UntypedExpr
parseLetRec = do
    _ <- symbol "letrec"
    fname <- parseIde
    _ <- symbol ":"
    t <- typ
    arg <- parseIde
    _ <- symbol ":"
    targ <- typ
    _ <- symbol "="
    fun <- expr
    _ <- symbol "in"
    body <- expr
    return (ELetRec fname t arg targ fun body) 

-- Function definition
parseFun :: Parser UntypedExpr
parseFun = do
    _ <- symbol "fun"
    ide <- parseIde
    _ <- symbol ":"
    t <- typTerm
    _ <- symbol "->"
    e <- expr
    return (EFun ide t e)

-- Atomic expressions (no operators or application)
term :: Parser UntypedExpr
term = choice
  [ parseInt
  , parseBool
  , parseIsZero
  , parseIf
  , parseLetRec
  , parseLet
  , parseFun
  , parseVar
  , parens expr
  ]

-- Application parser (left-associative)
parseApp :: Parser UntypedExpr
parseApp = do
  f <- term
  args <- many term
  return (foldl EApp f args)

parseIsZero :: Parser UntypedExpr
parseIsZero = do
    _ <- symbol "iszero"
    e <- expr
    return (EIsZero e)


-- Operators table for arithmetic
operatorTable :: [[Operator Parser UntypedExpr]]
operatorTable =
  [ [ InfixL (EMul <$ symbol "*"), InfixL (EDiv <$ symbol "/") ]
  , [ InfixL (EAdd <$ symbol "+"), InfixL (ESub <$ symbol "-") ]
  ]

-- Main expression parser
expr :: Parser UntypedExpr
expr = makeExprParser parseApp operatorTable

-- Top-level runParser
runParser :: String -> Either String UntypedExpr
runParser input = case parse (sc *> expr <* eof) "<input>" input of
    Left err -> Left (errorBundlePretty err)
    Right ast -> Right ast
