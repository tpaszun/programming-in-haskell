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
-- auto:
-- many
-- some


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
     e <- expr' (Term t)
     return e

expr' :: Expr -> Parser Expr
expr' e =
  do symbol "+"
     t <- term
     expr' (Sum e t)
   <|>
     do symbol "-"
        t <- term
        expr' (Difference e t)
   <|> return e

term :: Parser Term
term =
  do f <- factor
     t <- term' (Factor f)
     return t

term' :: Term -> Parser Term
term' t =
  do symbol "*"
     f <- factor
     term' (Product t f)
   <|>
     do symbol "/"
        f <- factor
        term' (Quotient t f)
   <|> return t


factor :: Parser Factor
factor =
  do e <- primitive
     do symbol "^"
        f <- factor
        return (Power e f )
      <|> return (Primitive e)

primitive :: Parser Primitive
primitive =
  do symbol "-"
     e <- expr
     return (Negation e)
   <|>
    do n <- integer
       return (Num n)
     <|>
      do symbol "("
         e <- expr
         symbol ")"
         return (Expression e)



eval :: String -> Expr
eval input = case parse expr input of
               [(n,[])] -> n
               [(_,out)] -> error ("Unused input: " ++ out)
               [] -> error "Invalid input"

data Expr = Term Term
          | Sum Expr Term
          | Difference Expr Term
          deriving (Show, Eq)

data Term = Factor Factor
          | Product Term Factor
          | Quotient Term Factor
          deriving (Show, Eq)

data Factor = Primitive Primitive
            | Power Primitive Factor
            deriving (Show, Eq)

data Primitive = Num Int
               | Negation Expr
               | Expression Expr
               deriving (Show, Eq)


exprToTreeFull :: Expr -> Tree String
exprToTreeFull (Term t) = Node "expr" [termToTreeFull t]
exprToTreeFull (Sum e t) = Node "expr" [Node "+" [exprToTreeFull e, termToTreeFull t]]
exprToTreeFull (Difference e t) = Node "expr" [Node "-" [exprToTreeFull e, termToTreeFull t]]

termToTreeFull :: Term -> Tree String
termToTreeFull (Factor f) = Node "term" [factorToTreeFull f]
termToTreeFull (Product t f) = Node "term" [Node "*" [termToTreeFull t, factorToTreeFull f]]
termToTreeFull (Quotient t f) = Node "term" [Node "/" [termToTreeFull t, factorToTreeFull f]]

factorToTreeFull :: Factor -> Tree String
factorToTreeFull (Primitive e) = Node "factor" [primitiveToTreeFull e]
factorToTreeFull (Power e f) = Node "factor" [Node "^" [primitiveToTreeFull e, factorToTreeFull f]]

primitiveToTreeFull :: Primitive -> Tree String
primitiveToTreeFull (Num n) = Node "primitive" [Node (show n) []]
primitiveToTreeFull (Negation e) = Node "primitive" [Node "-" [exprToTreeFull e]]
primitiveToTreeFull (Expression e) = Node "primitive" [exprToTreeFull e]


exprToTree :: Expr -> Tree String
exprToTree (Term t) = termToTree t
exprToTree (Sum e t) = Node "+" [exprToTree e, termToTree t]
exprToTree (Difference e t) = Node "-" [exprToTree e, termToTree t]

termToTree :: Term -> Tree String
termToTree (Factor f) = factorToTree f
termToTree (Product t f) = Node "*" [termToTree t, factorToTree f]
termToTree (Quotient t f) = Node "/" [termToTree t, factorToTree f]

factorToTree :: Factor -> Tree String
factorToTree (Primitive e) = primitiveToTree e
factorToTree (Power e f) = Node "^" [primitiveToTree e, factorToTree f]

primitiveToTree :: Primitive -> Tree String
primitiveToTree (Num n) = Node (show n) []
primitiveToTree (Negation e) = Node "-" [exprToTree e]
primitiveToTree (Expression e) = exprToTree e


