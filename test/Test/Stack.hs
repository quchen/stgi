module Test.Stack (tests) where

import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Test.Orphans          ()
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           Stack

tests :: TestTree
tests = testGroup "Stack" [test_popN, fromToList, test_mappend]

test_popN :: TestTree
test_popN = testGroup "popN"
    [ againstReference
    , roundtrip
    , negativeFirstArg ]
  where
    againstReference = QC.testProperty
        "Agrees with reference implementation"
        (\(NonNegative n) xs ->
            let _ = xs :: Stack Int
            in popN n xs === popN_reference n xs )
      where
        popN_reference :: Int -> Stack a -> Maybe ([a], Stack a)
        popN_reference n stack | n > length stack = Nothing
        popN_reference n stack = Just (let (xs, ys) = splitAt n (toList stack)
                                       in (xs, fromList ys) )

    roundtrip = QC.testProperty
        "pop-then-push"
        (\(NonNegative n) xs ->
            let popped = popN n xs
                _ = xs :: Stack Int
            in classify (isNothing popped) "Nothing"
                (case popped of
                    Just (a,b) -> a <>> b === xs
                    Nothing -> property True ))

    negativeFirstArg = QC.testProperty
        "Negative first argument => Nothing"
        (\(Positive n) xs ->
            let _ = xs :: Stack Int
            in popN (-n) xs === Nothing )


fromToList :: TestTree
fromToList = testGroup "List conversion"
    [ QC.testProperty
        "toList . fromList = id"
        (\xs -> let _ = xs :: [Int]
                in toList (fromList xs) === xs )
    , QC.testProperty
        "fromList . toList = id"
        (\xs -> let _ = xs ::Stack Int
                in fromList (toList xs) === xs )]

test_mappend :: TestTree
test_mappend = QC.testProperty
    "mappend = mappend for lists"
    (\xs ys -> let _ = xs :: Stack Int
               in xs <> ys === fromList (toList xs <> toList ys) )
