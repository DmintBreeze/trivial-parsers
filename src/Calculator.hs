module Calculator
  ( calculate
  , parseExpr
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

calculate :: String -> IO ()
calculate input = do
  let result = parseExpr input
  case result of
    Left e -> putStrLn $ show e
    Right n -> putStrLn $ show n

parseExpr :: String -> Either ParseError Float
parseExpr = parse expr "Simple Calculator"

-- syntax of expressions
-- digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- <number> ::= <digit> | <number><digit>
-- <expr> ::= <expr> + <term> | <expr> - <term> | <term>
-- <term> ::= <term> * <factor> | <term> / <factor> | <factor>
-- <factor> ::= <number> | ( <expr> )

expr = spaces *> term `chainl1` addOrSub
term = factor `chainl1` mulOrDiv
factor = parens expr <|> number

addOrSub = addop <|> minop
mulOrDiv = divop <|> mulop


lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

number :: Parser Float
number = lexeme $ toNumber <$> many1 digit
  where
    toNumber = read :: String -> Float

symbol :: Char -> Parser Char
symbol = lexeme . char

parens :: Parser a -> Parser a
parens = between (symbol '(') (symbol ')')


type Operator = Float -> Float -> Float

addop :: Parser Operator
addop = (+) <$ symbol '+'

minop :: Parser Operator
minop = (-) <$ symbol '-'

mulop :: Parser Operator
mulop = (*) <$ symbol '*'

divop :: Parser Operator
divop = (/) <$ symbol '/'

