module Chapter4 where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, length, nub, null, (..), (:))
import Data.Array.Partial (tail, head)
import Data.Foldable (foldl, product)
import Partial.Unsafe (unsafePartial)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Extra stuff. I just wanted to implement that.
-- This comes from SICP.
fact' :: Int -> Int
fact' num = factIter 1 1 num
  where
    factIter :: Int -> Int -> Int -> Int
    factIter prod count max
      | count > max = prod
      | otherwise   = factIter (prod * count) (count + 1) max
    
length' :: forall a. Array a -> Int
length' arr = if null arr
              then 0
              else 1 + length' (unsafePartial tail arr)

-- Exercise 4.4.1
-- Rewritten to use infix mod operator, as per section 4.6
isEven :: Int -> Boolean
isEven num = num `mod` 2 == 0

-- Exercise 4.4.2
numEvenIntegers :: Array Int -> Int
numEvenIntegers [] = 0
numEvenIntegers arr = if (isEven <<< unsafePartial head) arr
                      then 1 + (numEvenIntegers <<< unsafePartial tail) arr
                      else (numEvenIntegers <<< unsafePartial tail) arr



-- Exercise 4.7.1
squaresOf :: Array Int -> Array Int
squaresOf array = map (\n -> n * n) array

-- Exercise 4.7.2
removeNegatives :: Array Int -> Array Int
removeNegatives array = filter (\n -> n >= 0) array

-- Exercise 4.7.3
-- infix PRECEDENCE(0-9) FUNCTION as ALIAS
-- works as well for infixl and infixr
infix 8 filter as <$?>
removeNegativesInfix :: Array Int -> Array Int
removeNegativesInfix array = (\n -> n >= 0) <$?> array



-- Extra: I was wondering about creating a function for
-- generating numbers to test the negative-removing functions
-- above. Turns out concatMap is the function for that.
intercNegs :: Array Int -> Array Int
intercNegs nums = nub $ concatMap (\n -> [n, -n]) nums

-- removeNegatives $ intercNegs [1,2,3]



factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
  i <- 1..n
  j <- i..n
  [[i, j]]

-- Move filter inside array comprehension by using guard
factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1..n
  j <- i..n
  guard $ i * j == n
  pure [i, j]

-- Extra: Small test, uses isPrime from exercise 4.11.1
primesUpTo :: Int -> Array Int
primesUpTo n = do
  i <- 1..n
  guard $ isPrime i
  pure i



-- Exercise 4.11.1
isPrime :: Int -> Boolean
isPrime n = not(n == 1) && (factors' n) == [[1, n]]

-- Exercise 4.11.2
cartesianProduct :: Array Number -> Array Number -> Array (Array Number)
cartesianProduct a b = do
  i <- a
  j <- b
  pure [i, j]

-- Exercise 4.11.3
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1..n
  b <- a..n
  c <- b..n
  guard $ (square a) + (square b) == (square c)
  pure [a, b, c]
  where
    square :: Int -> Int -- redundant, I know.
    square m = m * m


-- Exercise 4.11.4
-- I am still not satisfied with this, but I'll come back to it later
factorizations :: Int -> Array (Array Int)
factorizations n = do
  fac1 <- 1..n
  guard $ not (fac1 == n) && n `mod` fac1 == 0
  let res = fac1 : (factor (n / fac1) fac1)
  guard $ (length res) > 1
  pure res
  where
    factor :: Int -> Int -> Array Int
    factor m fac1 = if m == 1 then [] else do
      fac2 <- m..n
      guard $ (fac2 > fac1) && (m `mod` fac2 == 0)
      fac2 : (factor (m / fac2) fac1)



-- An attempt in redefinition.
-- Notice the trick in fact'' definition
fact'' :: Int -> Int
fact'' = factIter 1
  where
    factIter :: Int -> Int -> Int
    factIter acc 0 = acc
    factIter acc n = factIter (n - 1) (acc * n)



-- Exercise 4.15.1
allTrue :: Array Boolean -> Boolean
allTrue = foldl (\xs x -> xs && x) true

-- Exercise 4.15.2
-- This function is for a boolean array containing only false.
allFalse :: Array Boolean -> Boolean
allFalse = foldl (==) false

-- Exercise 4.15.3
count :: forall a. (a -> Boolean) -> Array a -> Int
count = count' 0
  where
    count' :: Int -> (a -> Boolean) -> Array a -> Int
    count' acc _ [] = acc
    count' acc pred xs = do
      let newacc = if pred (unsafePartial head xs)
                   then acc + 1
                   else acc
      count' newacc pred (unsafePartial tail xs)
                            
-- Exercise 4.15.4
reverse' :: forall a. Array a -> Array a
reverse' = foldl (\acc n -> n : acc) []



-- NOTE: Exercises 4.17.1 thru 4.17.3 are still pending.
-- The main reason is because the Data.Path module is OLD,
-- and still needs a reimplementation for the latest compiler.
-- Since downloading non-updated, 2014 code is not a good thing,
-- I'll just leave this later. It seems like someone forked and
-- improved it, but I don't think I'll miss much from those
-- exercises.

