
module CheckBoundedSet where

import Test.QuickCheck
import BoundedSet

forAllMembers :: Testable prop => BoundedSet -> (Int -> prop) -> Property
forAllMembers s p =
  case boundedSetToList s
  of [] -> property True
     xs -> forAll (elements xs) p

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

closure_closed (Blind f) =
  let tc = bsTransitiveClosure f
  in tc `bsEqual` bsUnion tc (f (tc))

main = do
  quickCheck singleton_contains
  quickCheck singleton_excludes
  quickCheck union_subsumes
  quickCheck union_excludes
  quickCheck intersection_includes
  quickCheck intersection_excludes
  quickCheck difference_includes
  quickCheck difference_excludes
  quickCheck difference_union
  quickCheck closure_closed
