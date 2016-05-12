module Test.Stack (tests) where



import Data.Foldable
import Data.Monoid
import GHC.Exts              (fromList)
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Stack

import Test.Orphans ()



tests :: TestTree
tests = testGroup "Stack" [test_popN, fromToList, test_mappend]

test_popN :: TestTree
test_popN = testGroup "forEachPop"
    [ againstReference
    , roundtrip ]
  where
    againstReference = QC.testProperty
        "Agrees with naive implementation"
        (\xs stack ->
            let _ = stack :: Stack Int
                _ = xs :: [()]
            in xs `forEachPop` stack === xs `naive` stack )
      where
        naive :: [x] -> Stack a -> Maybe ([a], Stack a)
        naive xs stack
            | length xs > length stack = Nothing
            | otherwise =
                let (before, after) = splitAt (length xs) (toList stack)
                in Just (before, fromList after)

    roundtrip = QC.testProperty
        "pop-then-push"
        (\xs stack ->
            let popped = xs `forEachPop` stack
                _ = stack :: Stack Int
                _ = xs :: [Int]
            in case popped of
                Just (a,b) -> a <>> b === stack
                Nothing -> classify True "overpopped"
                    (length xs > length stack) )

fromToList :: TestTree
fromToList = testGroup "List conversion"
    [ QC.testProperty
        "toList . fromList = id"
        (\xs -> let _ = xs :: [Int]
                in fromStack (toStack xs) === xs )
    , QC.testProperty
        "fromList . toList = id"
        (\xs -> let _ = xs :: [Int]
                in fromStack (toStack xs) === xs )]
  where
    toStack = fromList :: [Int] -> Stack Int
    fromStack = toList :: Stack Int -> [Int]

test_mappend :: TestTree
test_mappend = QC.testProperty
    "mappend = mappend for lists"
    (\xs ys -> let _ = xs :: Stack Int
               in xs <> ys === fromList (toList xs <> toList ys) )
