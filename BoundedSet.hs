{- Define operations on bounded sets.
-}

module BoundedSet where

import Control.Applicative hiding(empty)
import Debug.Trace
import Test.QuickCheck
import Set

-- | A set of numbers within some bound.
--   @x@ is in @BoundedSet b s@ if @0 <= x <= b@ and @x@ is in @s@.
--
--   Because equality is based on membership, two bounded sets may be equal
--   even if they have different bounds.
data BoundedSet = BoundedSet Int Set

-- | Bounded sets can be randomly generated
instance Arbitrary BoundedSet where
  arbitrary = BoundedSet <$> sized (\s -> choose (0, s+1)) <*> arbitrary

-- | Functions on bounded sets can be randomly generated
instance CoArbitrary BoundedSet where
  coarbitrary bs = coarbitrary $ boundedSetToList bs

instance Show BoundedSet where
  show bs = "(boundedSetFromList " ++ show (boundedSetToList bs) ++ ")"

infix 4 `bsMember`, `bsNotMember`

-- | Get the upper bound of a bounded set
bsBound :: BoundedSet -> Int
bsBound (BoundedSet n _) = n

-- | Resize a bounded set.
--   The members of the new set are those that are smaller than both
--   the old and new bounds.
bsResize :: BoundedSet -> Int -> BoundedSet
bsResize _ _ = undefined

-- | Test whether an integer is a member of a set
bsMember :: Int -> BoundedSet -> Bool
bsMember n (BoundedSet b s) = n >= 0 && n <= b && n `member` s

-- | Test whether an integer isn't a member of a set
bsNotMember :: Int -> BoundedSet -> Bool
bsNotMember n s = not $ bsMember n s

-- | Get the members of a bounded set
boundedSetToList :: BoundedSet -> [Int]
boundedSetToList (BoundedSet b s) = [x | x <- [0..b], x `member` s]

-- | Create a bounded set of the given list
boundedSetFromList :: [Int] -> BoundedSet
boundedSetFromList xs = bsUnions $ map bsSingleton xs

bsSingleton :: Int -> BoundedSet
bsSingleton _ = undefined

-- | The empty set
bsEmpty :: BoundedSet
bsEmpty = BoundedSet 0 empty

-- | The set of all integers within the bound
bsFull :: Int -> BoundedSet
bsFull n = BoundedSet n (complement empty)

infix 4 `bsEqual`

-- | Test whether two bounded sets contain the same elements.
--   Note that the bounds do not have to be equal.
bsEqual :: BoundedSet -> BoundedSet -> Bool
bsEqual (BoundedSet b1 s1) (BoundedSet b2 s2) =
  all same_membership [0..max b1 b2]
  where
    same_membership x = (x `member` s1) == (x `member` s2)

-- | The union of two bounded sets
bsUnion :: BoundedSet -> BoundedSet -> BoundedSet
bsUnion _ _ = undefined

-- | The intersection of two bounded sets
bsIntersection :: BoundedSet -> BoundedSet -> BoundedSet
bsIntersection _ _ = undefined

-- | @s `difference` t@ is the elements of s that are not in t
bsDifference :: BoundedSet -> BoundedSet -> BoundedSet
bsDifference _ _ = undefined

-- | The union of a list of bounded sets
bsUnions :: [BoundedSet] -> BoundedSet
bsUnions = undefined

-- | Apply a function to each element of a bounded set an return the new
--   set.  Values outside the given bound are excluded from the set.
bsMap :: (Int -> Int) -> Int -> BoundedSet -> BoundedSet
bsMap f new_b (BoundedSet b s) =
  bsUnions [bsSingleton y
           | x <- [0..b], x `member` s, let y = f x, 0 <= y && y <= new_b]

-- | The transitive closure of a function on bounded sets.
--
--   The transitive closure of a function @f@ is the value obtained by
--   iterated application of @f@ to 'bsEmpty' until a fixed point is reached.
bsTransitiveClosure :: (BoundedSet -> BoundedSet) -> BoundedSet
bsTransitiveClosure _ = undefined
