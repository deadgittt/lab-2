{-# LANGUAGE ScopedTypeVariables #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module Main (main) where

import Bag
import Data.List (nub)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function (Fun (..))

instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
    arbitrary = fromList <$> arbitrary
    shrink b = fromList <$> shrink (toList b)

main :: IO ()
main = hspec $ do
    describe "Basic Bag Int tests" $ do
        it "singleton has size 1" $
            size (singleton (1 :: Int)) `shouldBe` 1

        it "insert increases count" $
            property $ \(x :: Int) (bag :: Bag Int) ->
                count x (insert x bag) == count x bag + 1

        it "deleteOne decreases count (floored at 0)" $
            property $ \(x :: Int) (bag :: Bag Int) ->
                let c = count x bag
                    c' = count x (deleteOne x bag)
                 in c' == max 0 (c - 1)

        it "deleteAll removes element completely" $
            property $ \(x :: Int) (bag :: Bag Int) ->
                count x (deleteAll x bag) == 0

        it "size matches length of toList" $
            property $ \bag ->
                size bag == length (toList (bag :: Bag Int))

        it "distinctSize matches nub of toList" $
            property $ \bag ->
                distinctSize bag == length (nub (toList (bag :: Bag Int)))

        it "toList/fromList roundtrip (multiset equality)" $
            property $ \xs ->
                fromList (xs :: [Int]) == fromList (toList (fromList xs))

        it "filterBag behaves like filter+fromList" $
            property $ \(bag :: Bag Int) (Fun _ (p :: Int -> Bool)) ->
                filterBag p bag == fromList (filter p (toList bag))

        it "mapBag behaves like map+fromList" $
            property $ \(bag :: Bag Int) (Fun _ (f :: Int -> Int)) ->
                mapBag f bag == fromList (map f (toList bag))

    describe "Char Unit-tests" $ do
        let bagStr = fromList "abca" :: Bag Char
        it "counts characters correctly" $
            count 'a' bagStr `shouldBe` 2
        it "deleteOne removes a single occurrence" $
            count 'a' (deleteOne 'a' bagStr) `shouldBe` 1
        it "deleteAll removes all occurrences" $
            count 'a' (deleteAll 'a' bagStr) `shouldBe` 0
        it "size and distinctSize match expectations" $ do
            size bagStr `shouldBe` 4
            distinctSize bagStr `shouldBe` 3
        it "member detects presence/absence" $ do
            member 'c' bagStr `shouldBe` True
            member 'z' bagStr `shouldBe` False
        it "toList renders sorted multiplicities" $
            toList bagStr `shouldBe` "aabc"
        it "union combines multiplicities" $
            let b1 = fromList "aa" :: Bag Char
                b2 = fromList "ab" :: Bag Char
             in count 'a' (b1 <> b2) `shouldBe` 3

    describe "Int Unit-tests" $ do
        let bagNums = fromList [1, 2, 2, 5] :: Bag Int
        it "size and distinctSize for ints" $ do
            size bagNums `shouldBe` 4
            distinctSize bagNums `shouldBe` 3
        it "counts occurrences of an int" $
            count 2 bagNums `shouldBe` 2
        it "deleteOne reduces int multiplicity" $
            count 2 (deleteOne 2 bagNums) `shouldBe` 1
        it "deleteAll removes int completely" $
            count 2 (deleteAll 2 bagNums) `shouldBe` 0
        it "member works for ints" $ do
            member 5 bagNums `shouldBe` True
            member 9 bagNums `shouldBe` False
        it "toList for ints returns sorted with multiplicity" $
            toList bagNums `shouldBe` [1, 2, 2, 5]

    describe "Additional Eq tests (because of new implementation)" $ do
        let bagA = foldl (flip insert) empty ([2, 1, 3, 2] :: [Int])
            bagB = insert 3 (insert 2 (insert 1 (insert 2 empty))) :: Bag Int
            bagC = fromList [1, 2, 3] :: Bag Int
        it "bags with same multiset but different shape are equal" $
            bagA `shouldBe` bagB
        it "bags with different multisets are not equal" $
            bagA `shouldNotBe` bagC

    describe "Monoid axioms" $ do
        it "left identity" $
            property $ \bag ->
                (mempty <> bag :: Bag Int) == bag

        it "right identity" $
            property $ \bag ->
                (bag <> mempty :: Bag Int) == bag

        it "associativity" $
            property $ \a b c ->
                ((a <> b) <> c :: Bag Int) == (a <> (b <> c))

        it "union adds counts" $
            property $ \a b x ->
                count x ((a <> b) :: Bag Int) == count x a + count x b
