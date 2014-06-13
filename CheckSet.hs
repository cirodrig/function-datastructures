
module CheckSet where

import Test.QuickCheck
import Set

-- Properties that should be tautologies
singleton_contains x = x `member` singleton x

empty_not_contains x = x `notMember` empty

complement_opposite (Blind s) = forAll arbitrary $ \x ->
  x `notMember` intersection s (complement s)

union_subsumes (Blind s) (Blind t) = forAll arbitrary $ \x ->
  x `member` union s t ==> x `member` s || x `member` t

union_excludes (Blind s) (Blind t) = forAll arbitrary $ \x ->
  x `notMember` s && x `notMember` t ==> x `notMember` union s t

intersection_includes (Blind s) (Blind t) = forAll arbitrary $ \x ->
  x `member` s && x `member` t ==> x `member` intersection s t

intersection_excludes (Blind s) (Blind t) = forAll arbitrary $ \x ->
  x `notMember` intersection s t ==> x `notMember` s || x `notMember` t

difference_excludes (Blind s) (Blind t) = forAll arbitrary $ \x ->
  x `member` t ==> x `notMember` difference s t

difference_includes (Blind s) (Blind t) = forAll arbitrary $ \x ->
  x `member` difference s t ==> x `member` s

difference_union (Blind s) (Blind t) = forAll arbitrary $ \x ->
  x `notMember` union s t ==> x `notMember` difference s t

unions_equals (Blind ss) = forAll arbitrary $ \x ->
  (x `member` unions ss) == any (x `member`) ss

main = do
  putStrLn "singleton"
  quickCheck singleton_contains
  putStrLn "empty"
  quickCheck empty_not_contains
  putStrLn "complement"
  quickCheck complement_opposite
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
  putStrLn "unions"
  quickCheck unions_equals
