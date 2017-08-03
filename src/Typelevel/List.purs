module Data.Typelevel.List where

import Data.Typelevel.Num (D0, reifyInt)
import Data.Typelevel.Num.Ops(class Succ)
import Data.Typelevel.Undefined (undefined)

data TNil
data TList a b

class Contains a b
instance containsRecursive :: (Contains b c) => Contains (TList a b) c
instance containsEqual :: Contains (TList a b) a

add :: forall a. (Contains (TList Int (TList Number (TList String TNil))) a) => a -> Int
add a = 2

addNumber = add 2.0
addInt = add 2
addString = add "2"

class Prepend a b c | a b -> c, a c -> b, b c -> a
instance prependLists :: Prepend a (TList b c) (TList a (TList b c))

subs :: forall a b. Prepend Int (TList Number TNil) a => Contains a b => b -> Int
subs a = 2

subsInt = subs 2
subsNumber = subs 2.0


class Merge a b c | a b -> c, a c -> b, b c -> a
instance mergeNothings :: Merge TNil TNil TNil
instance mergeNothing1 :: Merge (TList a b) (TList a b) TNil
instance mergeNothing2 :: Merge (TList a b) TNil (TList a b) 
instance mergeLists :: Merge b d (TList e f) => Merge (TList a b) (TList a d) (TList e f)

merge :: forall a b. Merge a (TList Number TNil) (TList Int TNil) => Contains a b => b -> Int
merge a = 2

mergeInt = merge 2
mergeNumber = merge 2.0

class Length a b | a -> b
instance nilLength :: Length TNil D0
instance someLength :: (Length b c, Succ c d) => Length (TList a b) d

lengthSome :: forall a b. Length a b => b -> Int
lengthSome b = 2

class ReverseHelper a b c
instance reverseNil :: ReverseHelper TNil TNil TNil

class Reverse a b | a -> b, b -> a
instance helpedReverse :: ReverseHelper a b c => Reverse a c

-- Remove
-- Sort
-- Modify!!!!
