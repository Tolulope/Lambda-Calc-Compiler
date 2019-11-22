import Control.Applicative
import Data.Char
import Parsing
--Challenge 1 Lambda Calculus Utility Functions
--λ
--Challenge 1a Free Variables

data Expr = App Expr Expr | Lam Int Expr | Var Int
    deriving(Show, Eq)

freeVariables :: Expr -> [Int]
freeVariables e = fst (findFree [] e [] 0)

findFree :: [Int] -> Expr -> [(Int,Int)] -> Int -> ([Int],[(Int,Int)])
findFree xs (App e1 e2) ys c = findFree (fst initialEx) e2 (snd initialEx) c'
                               where initialEx = findFree xs e1 ys c
                                     c' = snd (head(snd(initialEx)))
findFree xs (Lam a e) ys c = findFree xs e (ys++[(a, c-1)]) (c-1)
findFree xs (Var a) ys c | elem a [b | (b,c) <- ys] = (xs,ys)
                         | otherwise = ((xs++[a]),ys)

--Challenge 1b Rename
rename :: Expr -> Int -> Int -> Expr
rename e c new | numLam == [] = e
               | otherwise = renameCons (removeByPass one (c2) new) (tail boundInit)
       where    boundInit = ((0,0):(snd(findFree [] e [] 0)))
                one = removeNest e boundInit boundInit 0
                bound = (snd(findFree [] one [] 0))
                numLam = [b | (a,b) <- boundInit, a==c]
                c2 = head numLam


removeNest :: Expr -> [(Int,Int)] -> [(Int,Int)] -> Int-> Expr
removeNest (Var x) tailB bound l | elem x [ a | (a,b)<-tmp] = Var (head([b | (a,b) <-tmp, x==a])) 
                                    | otherwise = Var x
                                where tmp | elem l [b|(a,b) <- tailB] = tailB
                                          | otherwise = bound
removeNest (App e1 e2) tailB bound l = (App (removeNest e1 tailB bound l) (removeNest e2 tailB bound l))
removeNest (Lam y e) tailB bound l = Lam (head [b| (a,b) <- (tail tailB), a == y]) (removeNest e (tail tailB) bound l')
          where l' = (head [b| (a,b) <- (tail tailB), a == y])


removeByPass :: Expr -> Int -> Int -> Expr
removeByPass (Var x) c new | x == c = Var new
                           |otherwise = Var x
removeByPass (App e1 e2) c new = (App (removeByPass e1 c new) (removeByPass e2 c new))
removeByPass (Lam y e) c new  | y == c  = (Lam new (removeByPass e c new))
                              | otherwise = (Lam y (removeByPass e c new))

renameCons :: Expr -> [(Int,Int)] -> Expr
renameCons (Var x) bound | elem x [b| (a,b) <- bound] = Var (head[a|(a,b)<-bound, x==b])
                      | otherwise = Var x
renameCons (App e1 e2) bound = (App (renameCons e1 bound) (renameCons e2 bound))
renameCons (Lam y e) bound | elem y [b| (a,b) <- bound] = Lam (head[a|(a,b)<-bound, y==b]) (renameCons e bound)
                        | otherwise = (Lam y (renameCons e bound))


--Challenge 1c Alpha Equivalent
alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent e1 e2 = (removeNest e1 bound1 bound1 0) == (removeNest e2 bound2 bound2 0)
                where bound1 = ((0,0):(snd(findFree [] e1 [] 0)))
                      bound2 = ((0,0):(snd(findFree [] e2 [] 0)))


--Challenge 1d has Reducable Expression
hasRedex :: Expr -> Bool
hasRedex (Var x) = False
hasRedex (App (Lam x e1) e2) = True
hasRedex (App e1 e2) = hasRedex e1 || hasRedex e2
hasRedex (Lam x e) = hasRedex e

--Challenge 1e substitute
substitute :: Expr -> Int -> Expr -> Expr
substitute (Var x) n sub | n == x = sub
                         | otherwise = (Var x)
substitute (App e1 e2) n sub = (App (substitute e1 n sub) (substitute e2 n sub))
substitute (Lam y e) n sub | n == y = (Lam y e)
                           | elem y (fst(subVar)) = substitute (rename (Lam y e) y newI) n sub 
                           | otherwise = (Lam y (substitute e n sub))
                                    where subVar = findFree [] sub [] 0
                                          eVar = findFree [] e [] 0
                                          max = bopToTheTop (fst(subVar) ++ fst(eVar) ++ [a|(a,b)<-snd subVar] ++ [a|(a,b)<-snd eVar])
                                          newI = max + 1 
bopToTheTop :: [Int] -> Int
bopToTheTop (x:[]) = x
bopToTheTop (x:y:xs) | x > y = bopToTheTop (x:xs)
              | otherwise = bopToTheTop (y:xs)


-- Challenge 2 - Pretty Printer
prettyPrint::Expr -> String
prettyPrint (Var a) = "x"++(show a)++""
prettyPrint (App (Var a) (Var b))  = ""++prettyPrint (Var a)++""++prettyPrint (Var b)++""
prettyPrint(App (Var a) e2) = ""++prettyPrint (Var a)++"("++prettyPrint e2++")"
prettyPrint (App e1 e2) = ""++prettyPrint e1++""++prettyPrint e2++""
prettyPrint (Lam a (Lam b e)) = "\\x"++(show a)++"x"++(show b)++"->"++(prettyPrint e)++""
prettyPrint (Lam a e) = "\\x"++(show a)++"->"++(prettyPrint e)++""

-- Challenge 3 - Lambda Calaculus Parser
data ExtExpr = ExtApp ExtExpr ExtExpr | ExtLam [Int] ExtExpr | ExtVar Int
     deriving (Show, Eq)
     
parseLam:: String -> Maybe ExtExpr --return Nothing if the String does not parse correctly
parseLam str | pars == [] = Nothing
             | otherwise =  Just (fst (head pars)) 
                      where pars = parse lambdaExpr str


lambdaExpr::Parser ExtExpr
lambdaExpr = do 
                    par1 <-lamvar <|> calc <|> bracket 
                    par2 <- many lambdaExpr
                    if (par2 == ([])) then (return par1) else (return (ExtApp par1 (head par2)))

lamvar :: Parser ExtExpr
lamvar = do
  space
  char 'x'
  var <- integer
  space
  return(ExtVar var)

calc :: Parser ExtExpr
calc = do
      some (symbol "\\")
      var <-  vars
      symbol "-"
      symbol ">"
      body <- lambdaExpr
      space
      return(ExtLam (listify var) body)

vars :: Parser [ExtExpr]
vars = do 
    var <- some lamvar
    return var

listify::[ExtExpr] -> [Int]
listify [] = []
listify ((ExtVar i):ls) = [i]++listify ls

bracket :: Parser ExtExpr
bracket = do
  symbol "("
  term <- lambdaExpr
  symbol ")"
  return term


--Challenge 4 - Lambda Calculus to combinatory logic
data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int
     deriving (Show)

translate :: Expr -> ExprCL
translate (Var x) = VarCL x
translate (App e1 e2) = AppCL (translate e1) (translate e2)
translate (Lam a (Var x)) | a == x = (AppCL (AppCL S  K)  K)
                          | otherwise = VarCL x
translate (Lam x (App e1 e2)) | elem x (freeVariables e1) || elem x (freeVariables e2) = (AppCL S (AppCL (translate (Lam x e1)) (translate (Lam x e2))))
                              | otherwise = AppCL K (translate (App e1 e2))
translate (Lam x (Lam y e)) | not (elem x (freeVariables e)) = AppCL K (translate (Lam y e))
                            |otherwise = translate (Lam y e)