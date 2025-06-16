module Parser where

import UntypedExpr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Type
import Data.Type.Coercion (sym)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

boolean :: Parser Bool
boolean = (True <$ symbol "true")
    <|> (False <$ symbol "false")

typ :: Parser Type
typ = makeExprParser typTerm typOperators

typTerm :: Parser Type
typTerm = (TInt <$ symbol "int")
      <|> (TBool <$ symbol "bool")
      <|> parens typ

typOperators :: [[Operator Parser Type]]
typOperators =
  [ [InfixR (TFun <$ symbol "->")] ]

parseInt :: Parser UntypedExpr
parseInt = EInt <$> integer

parseBool :: Parser UntypedExpr
parseBool = EBool <$> boolean

parseVar :: Parser UntypedExpr
parseVar = do
    first <- letterChar
    rest <- many alphaNumChar
    return (EVar (first:rest))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: String -> Parser ()
keyword w = lexeme (string w) >> return ()

expr :: Parser UntypedExpr
expr = makeExprParser termNoApp operatorTable

operatorTable :: [[Operator Parser UntypedExpr]]
operatorTable =
  [ [ InfixL (EMul <$ symbol "*"), InfixL (EDiv <$ symbol "/") ]
  , [ InfixL (EAdd <$ symbol "+") ]
  ]

parseIf :: Parser UntypedExpr
parseIf = do
  _ <- keyword "if"
  cond <- expr
  _ <- keyword "then"
  thn <- expr
  _ <- keyword "else"
  els <- expr
  return (EIf cond thn els)

parseIde :: Parser String
parseIde = lexeme $ do
    first <- letterChar
    rest <- many alphaNumChar
    return (first:rest)

parseLet :: Parser UntypedExpr
parseLet = do
    keyword "let"
    ide <- parseIde
    symbol "="
    e1 <- expr
    keyword "in"
    e2 <- expr
    return (ELet ide e1 e2)


parseFun :: Parser UntypedExpr
parseFun = do
    _ <- keyword "fun"
    ide <- parseIde
    _ <- symbol ":"
    t <- typ
    _ <- symbol "->"
    e <- expr
    return (EFun ide t e)

parseApp :: Parser UntypedExpr
parseApp = do
    func <- termNoApp
    args <- many termNoApp
    return (foldl EApp func args)

parseLetRec :: Parser UntypedExpr
parseLetRec = do
    _ <- keyword "let"
    _ <- keyword "rec"
    ide1 <- parseIde
    _ <- symbol ":"
    t1 <- typ
    ide2 <- parseIde
    _ <- symbol ":"
    t2 <- typ
    _ <- symbol "="
    body <- expr
    _ <- keyword "in"
    e <- expr
    return (ELetRec ide1 t1 ide2 t2 body e)


termNoApp :: Parser UntypedExpr
termNoApp = parseInt
         <|> parseBool
         <|> parseIf
         <|> parseLet
         <|> parseLetRec
         <|> parseFun
         <|> parseVar
         <|> parens expr

runParser :: String -> Either String UntypedExpr
runParser input = case parse (sc *> expr <* eof) "<input>" input of
    Left msg -> Left (errorBundlePretty msg)
    Right ast -> Right ast