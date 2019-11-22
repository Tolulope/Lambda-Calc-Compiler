-- Hutton's arithmetic expressions, chapter 13 Programming in Haskell
-- This parser evaluates the expression rather than building a tree

import Control.Applicative
import Data.Char
import Parsing

-- Arithmetic Expressions
expr :: Parser Int
expr = do t <- term
          symbol "+"
          e <- expr
          return (t + e)
    <|>
       do t <- term
          return t

term :: Parser Int
term = do f <- factor
          symbol "*"
          t <- term
          return (f * t)
    <|>
       do f <- factor
          return f
       
factor :: Parser Int
factor = do symbol "(" 
            e <- expr
            symbol ")"
            return e
         <|> natural

parseArithmetic :: String -> Maybe Int
parseArithmetic s = 
    let ps = parse' expr s in
      if ps == [] then Nothing
      else let [(e,s')] = ps in 
        if s' /= "" then Nothing      
        else Just e

parse' :: Parser a -> String -> [(a,String)]
parse' (P p) inp = p inp

