module Parser where

import Control.Applicative
import Data.Char
import Data.Tree
import Data.Tree.Pretty

newtype Parser a = P (String -> [(a, String)])


parse :: Parser a -> String -> [(a, String)]
parse (P p)input = p input

item :: Parser Char
item = P (\input -> case input of
                      []     -> []
                      (x:xs) -> [(x, xs)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\input -> case parse p input of
                            []          -> []
                            [(a, rest)] -> [(f a, rest)])

instance Applicative Parser where
-- pure :: a -> Parser a
  pure v = P (\input -> [(v, input)])
-- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pl <*> pr = P (\input -> case parse pl input of
                             []          -> []
                             [(f, rest)] -> parse (fmap f pr) rest)

instance Monad Parser where
-- >>= :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\input -> case parse p input of
                           [] -> []
                           [(a, rest)] -> parse (f a) rest)

three :: Parser (Char, Char)
three = do x <- item
           item
           y <- item
           return (x,y)

instance Alternative Parser where
-- empty :: Parser a
  empty = P (\input -> [])
-- <|> :: Parser a -> Parser a -> Parser a
  l <|> r = P (\input -> case parse l input of
                           [] -> parse r input
                           [(x, rest)] -> [(x, rest)])


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","; natural)
          symbol "]"
          return (n:ns)

list :: Parser [Int]
list = do symbol "["
          xs <- do n <- natural
                   ns <- many (do symbol ","; natural)
                   return (n:ns)
                <|> empty
          symbol "]"
          return xs


expr :: Parser Expr
expr =
  do t <- term
     do symbol "+"
        e <- expr
        return (ESum t e)
      <|>
       do symbol "-"
          e <- expr
          return (EDif t e)
        <|> return (ETerm t)

term :: Parser Term
term =
  do f <- factor
     do symbol "*"
        t <- term
        return (TMul f t)
      <|>
       do symbol "/"
          t <- term
          return (TDiv f t)
        <|> return (TFactor f)

factor :: Parser Factor
factor =
  do e <- expnent
     do symbol "^"
        f <- factor
        return (FExpo e f )
      <|> return (FExp e)

expnent :: Parser Exponent
expnent =
  do n <- integer
     return (Num n)
    <|>
    do symbol "("
       e <- expr
       symbol ")"
       return (EExpr e)



eval :: String -> Expr
eval input = case parse expr input of
               [(n,[])] -> n
               [(_,out)] -> error ("Unused input: " ++ out)
               [] -> error "Invalid input"


data Expr = ETerm Term
          | ESum Term Expr
          | EDif Term Expr
          deriving Show

data Term = TFactor Factor
          | TMul Factor Term
          | TDiv Factor Term
          deriving Show

data Factor = FExp Exponent
            | FExpo Exponent Factor
            deriving Show

data Exponent = Num Int
              | EExpr Expr
              deriving Show


exprToTree :: Expr -> Tree String
exprToTree (ETerm t) = Node "expr" [termToTree t]
exprToTree (ESum t e) = Node "expr" [Node "+" [termToTree t, exprToTree e]]
exprToTree (EDif t e) = Node "expr" [Node "-" [termToTree t, exprToTree e]]

termToTree :: Term -> Tree String
termToTree (TFactor f) = Node "term" [factorToTree f]
termToTree (TMul f t) = Node "term" [Node "*" [factorToTree f, termToTree t]]
termToTree (TDiv f t) = Node "term" [Node "/" [factorToTree f, termToTree t]]

factorToTree :: Factor -> Tree String
factorToTree (FExp e) = Node "factor" [exponentToTree e]
factorToTree (FExpo e f) = Node "factor" [Node "^" [exponentToTree e, factorToTree f]]

exponentToTree :: Exponent -> Tree String
exponentToTree (Num n) = Node "exp" [Node (show n) []]
exponentToTree (EExpr e) = Node "exp" [exprToTree e]


