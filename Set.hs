{- Define operations on sets represented by characteristic functions.

'complement', 'union', 'intersection', and 'difference' should be
defined in terms of characteristic functions.  They should not be defined
in terms of other functions that operate on sets.
-}

module Set where

import Test.QuickCheck

-- | A set represented by its characteristic function.
--   @x@ is in @Set f@ iff @f x@.
newtype Set = Set (Int -> Bool)

-- | Sets can be randomly generated
instance Arbitrary Set where arbitrary = fmap Set arbitrary

infix 4 `member`, `notMember`

-- | Test whether an integer is a member of a set
member :: Int -> Set -> Bool
member n (Set f) = f n

-- | Test whether an integer isn't a member of a set
notMember :: Int -> Set -> Bool
notMember n s = not $ member n s

-- | A set containing one number
singleton :: Int -> Set
singleton n = Set (\x -> x == n)

-- | The empty set
empty :: Set
empty = undefined

-- | The inverse of a set
complement :: Set -> Set
complement (Set f) = undefined

-- | The union of two sets
union :: Set -> Set -> Set
union (Set f) (Set g) = undefined

-- | The intersection of two sets
intersection :: Set -> Set -> Set
intersection (Set f) (Set g) = undefined

-- | @s `difference` t@ is the elements of s that are not in t
difference :: Set -> Set -> Set
difference (Set f) (Set g) = undefined

-- | The union of a list of sets
unions :: [Set] -> Set
unions = undefined
