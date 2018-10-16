module Test where

import qualified Data.Set as Set
import Data.Set (Set)

import Test.QuickCheck (quickCheck)


mergeFirst :: (Int -> Int -> Int) -> Set Int -> Set Int -> Set Int -> Set Int -> Set Int
mergeFirst f y z y' z' = Set.fromList [f a b | (a, b) <- Set.toList $
                                        Set.cartesianProduct (Set.union y y') (Set.union z z')]

mergeAfter :: (Int -> Int -> Int) -> Set Int -> Set Int -> Set Int -> Set Int -> Set Int
mergeAfter f y z y' z' = Set.union s1 s2
  where
    s1 = Set.fromList [f a b | (a, b) <- Set.toList $ Set.cartesianProduct y z]
    s2 = Set.fromList [f a b | (a, b) <- Set.toList $ Set.cartesianProduct y' z']


monotonicPlus y z y' z' = let f = (+) in Set.isSubsetOf (mergeAfter f y z y' z') (mergeFirst f y z y' z')
monotonicMul y z y' z' = let f = (*) in Set.isSubsetOf (mergeAfter f y z y' z') (mergeFirst f y z y' z')
test_monotonicPlus = quickCheck monotonicPlus
test_monotonicMul = quickCheck monotonicMul
  -- (mergeFirst (+) y z y' z', mergeAfter (+) y z y' z')
  -- where
  --   y = Set.fromList [ 1, 2, 3 ]
  --   z = Set.fromList [ 1, 2, 3, 4, 5 ]
  --   y' = Set.fromList [ 1, 3, 6, 7 ]
  --   z' = Set.fromList [ 1, 6, 9 ]
