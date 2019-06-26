module Chapter5 where

import Prelude

import Data.Array.Partial (tail)
import Data.Traversable (sum)
import Partial.Unsafe (unsafePartial)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m | n > m     = gcd (n - m) m
        | otherwise = gcd n (m - n)

fromString :: String -> Boolean
fromString "true" = true
fromString _      = false

toString :: Boolean -> String
toString true  = "true"
toString false = "false"


factorial :: Int -> Int
factorial n = fact' 1 n
  where
    fact' :: Int -> Int -> Int
    fact' acc 0  = acc
    fact' acc it = fact' (acc * it) (it - 1)

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _  = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _               = 0

-- Important: this type specification allows for more fields.
showPerson :: forall r. { first :: String, last :: String | r } -> String
showPerson { first: x, last: y } = y <> ", " <> x

type Address = { street :: String, city :: String }
type Person  = { name :: String, address :: Address }

livesInLA :: forall r1 r2. { address :: { city :: String | r1 } | r2 } -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y     = arr
  | otherwise = [y, x]
sortPair arr = arr



-- Exercise 5.9.1
sameCity :: forall a r1 r2 r3 r4. Eq a =>
            { address :: { city :: a | r1 } | r2 } ->
            { address :: { city :: a | r3 } | r4 } ->
            Boolean
sameCity { address: addr1@{ city: x } } { address: addr2@{ city: y } }
  | x == y     = true
  | otherwise = false


{- Exercise 5.9.2
   For sameCity, the most general type is

   sameCity :: forall a r1 r2 r3 r4. Eq a =>
               { address :: { city :: a | r1 } | r2 } ->
               { address :: { city :: a | r3 } | r4 } ->
               Boolean

   This means that there must be two records with at least one "address" row,
   such that these rows are also a record containing at least a "city" row of
   the type a.
   Furthermore, the type a must be Eq'able. The result is a Boolean value.

   For livesInLA, the most general type is

   livesInLA :: forall r1 r2.
                { address :: { city :: String | r1 } | r2 } -> Boolean

   This means that there must be a record with at least an "address" row, such
   that this row is also a record containing at least a "city" row of type
   String. The result is a Boolean value.                                     -}


-- Exercise 5.9.3
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _   arr@[x] = x
fromSingleton def _       = def


-- sum comes from Data.Traversable
lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
  0 -> xs
  _ -> lzs (unsafePartial tail xs)


-- Partial function wrapped by unsafePartial
-- partialFunction :: Boolean -> Boolean
-- partialFunction = unsafePartial \true -> true

-- Explicitly partial function as given by type class Partial
partialFunction :: Partial => Boolean -> Boolean
partialFunction true = true

-- redundantCase :: Boolean -> Boolean
-- redundantCase true = true
-- redundantCase false = false
-- redundantCase false = false

{- See the rest in Data.Picture -}

