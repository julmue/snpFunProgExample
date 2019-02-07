module Main where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-- syntax (generative grammar): three value constructors
data Term =
    Var Integer
  | Add Term Term
  | Mul Term Term
  deriving (Show, Eq)

-- some test data
tvar = Var 42
tadd = Add (Var 1) (Var 41)
tmul = Mul (Var 2) (Var 43)
tdist = (Var 1 `Add` Var 2) `Mul` (Var 3)
tbig = (tadd `Mul` tdist) `Add` tmul

-- fold (generic evaluator): pattern matching against value constructors
foldTerm :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> Term -> a
foldTerm fVar _ _ (Var i) = fVar i
foldTerm fVar fAdd fMul (Add x y) = fAdd (foldTerm fVar fAdd fMul x) (foldTerm fVar fAdd fMul y)
foldTerm fVar fAdd fMul (Mul x y) = fMul (foldTerm fVar fAdd fMul x) (foldTerm fVar fAdd fMul y)

-- parser
lexer :: TokenParser ()
lexer = makeTokenParser emptyDef { reservedOpNames = ["+", "*"] }

term :: Parser Term
term = buildExpressionParser table (parens lexer term <|> var)
  where
    table = [ [binary "*" Mul AssocLeft]
            , [binary "+" Add AssocLeft]
            ]
    binary name func assoc = Infix (do { reservedOp lexer name; return func }) assoc
    var = Var <$> (natural lexer)

parseTerm :: String -> Either ParseError Term
parseTerm = runParser term () "<Arithmetic Expression>"

-- pretty Printer
prettyprintTerm :: Term -> String
prettyprintTerm = foldTerm showVar showAdd showMul
  where
    showVar = show
    showAdd x y = "(" ++ x ++ "+" ++ y ++ ")"
    showMul x y = "(" ++ x ++ "*" ++ y ++ ")"

-- evaluator
eval :: Term -> Integer
eval = foldTerm id (+) (*)

-- rewriting / simplification rules
-- distributivity: (x + y) * z = x * z + y * z
dist :: Term -> Term
dist ((x `Add` y) `Mul` z) = (dist x `Mul` dist z) `Add` (dist y `Mul` dist z)
dist (z `Mul` (x `Add` y)) = (dist x `Mul` dist z) `Add` (dist y `Mul` dist z)
dist (x `Add` y) = (dist x `Add` dist y)
dist (x `Mul` y) = (dist x `Mul` dist y)
dist (Var x) = Var x

-- testing our functions
genTerm :: Int -> Gen Term
genTerm depth
    | depth <= 1 =  Var <$> arbitrary
    | otherwise = do
        depth1 <- genDepth
        depth2 <- genDepth
        oneof [ genTerm 1
              , Add <$> genTerm depth1 <*> genTerm depth2
              , Mul <$> genTerm depth1 <*> genTerm depth2
              ]
  where
    genDepth = elements [1 .. pred depth]

instance Arbitrary Term where
  arbitrary = sized genTerm

-- now we can generate arbitrary Terms
randomizeTerm :: IO Term
randomizeTerm = generate arbitrary

-- check that distributivity doesn't change value
prop_dist :: Term -> Bool
prop_dist t1 = eval (dist t1) == eval t1

test :: IO ()
test = quickCheckWith stdArgs { maxSuccess = 5000 } prop_dist

main :: IO ()
main = putStrLn "a quick haskell tour ..."
