
module CheckBoundedSet where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Vector as V
import Test.QuickCheck

import BoundedSet

forAllMembers :: Testable prop => BoundedSet -> (Int -> prop) -> Property
forAllMembers s p =
  case boundedSetToList s
  of [] -> property True
     xs -> forAll (elements xs) p

-- | Generate a random function from bounded integers to bounded sets.
--   The function's output is biased toward sets of size @(1 - eps)/bound@.
--
--   From graph theory, interpreting the function as giving the edges of a
--   directed graph, this size is on the border of where the graph has
--   one giant component.
genSmallSetFunction :: Int -> Gen (Int -> BoundedSet)
genSmallSetFunction bound = do
  -- Precalculate the entire range of the function.
  range <- replicateM (bound + 1) $ boundedSetFromList <$> gen_set
  let rangev = V.fromList range

  -- Look up values from the range
  let f i | i >= 0 && i <= bound = rangev V.! i
          | otherwise            = error "Index out of bounds"
  rangev `seq` return f
  where
    prob :: Double
    prob = if bound == 0
           then 0
           else (1 - 0.002) / fromIntegral bound

    gen_set :: Gen [Int]
    gen_set = do
      liftM catMaybes $ forM [0..bound] $ \i -> do
        r <- choose (0, 1)
        return $! if r < prob then Just i else Nothing

genMonotonicBoundedFunction :: Gen (BoundedSet -> BoundedSet)
genMonotonicBoundedFunction = sized $ \sz -> do
  f <- genSmallSetFunction sz
  return $ bsUnions . map f . boundedSetToList

-- Properties that should be tautologies
singleton_contains x = x `bsMember` bsSingleton x

singleton_excludes x y = x /= y ==> y `bsNotMember` bsSingleton x

union_subsumes s t = forAllMembers (bsUnion s t) $ \x ->
  x `bsMember` s || x `bsMember` t

union_excludes s t = forAll arbitrary $ \x ->
  x `bsNotMember` s && x `bsNotMember` t ==> x `bsNotMember` bsUnion s t

intersection_includes s t = forAllMembers (bsUnion s t) $ \x ->
  x `bsMember` bsIntersection s t

intersection_excludes s t = forAll arbitrary $ \x ->
  x `bsNotMember` bsIntersection s t ==> x `bsNotMember` s || x `bsNotMember` t

difference_excludes s t = forAllMembers t $ \x ->
  x `bsNotMember` bsDifference s t

difference_includes s t = forAllMembers (bsDifference s t) $ \x ->
  x `bsMember` s

difference_union s t = forAll arbitrary $ \x ->
  x `bsNotMember` bsUnion s t ==> x `bsNotMember` bsDifference s t

closure_contains_f =
  forAll (fmap Blind genMonotonicBoundedFunction) $ \(Blind f) ->
  f bsEmpty `bsDifference` bsTransitiveClosure f `bsEqual` bsEmpty

closure_closed =
  forAll (fmap Blind genMonotonicBoundedFunction) $ \(Blind f) ->
  let tc = bsTransitiveClosure f
  in tc `bsEqual` bsUnion tc (f (tc))

main = do
  putStrLn "singleton"
  quickCheck singleton_contains
  quickCheck singleton_excludes
  putStrLn "union"
  quickCheck union_subsumes
  quickCheck union_excludes
  putStrLn "intersection"
  quickCheck intersection_includes
  quickCheck intersection_excludes
  putStrLn "difference"
  quickCheck difference_includes
  quickCheck difference_excludes
  quickCheck difference_union
  putStrLn "closure"
  quickCheck closure_contains_f
  quickCheck closure_closed
