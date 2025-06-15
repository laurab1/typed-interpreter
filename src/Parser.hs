module Parser where

import UntypedExpr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

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

expr :: Parser UntypedExpr
expr = makeExprParser term operatorTable

term :: Parser UntypedExpr
term = parseInt
    <|> parseBool
    <|> parseVar
    <|> parens expr

operatorTable :: [[Operator Parser UntypedExpr]]
operatorTable =
  [ [ InfixL (EMul <$ symbol "*") ]
  , [ InfixL (EDiv <$ symbol "/")]
  , [ InfixL (EAdd <$ symbol "+") ]
  ]
