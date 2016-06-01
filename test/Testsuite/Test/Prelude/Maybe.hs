{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Maybe (tests) where



import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.HaskellReference
import Test.Orphans                                         ()
import Test.Tasty



tests :: TestTree
tests = testGroup "Maybe"
    [ testMaybe ]

testMaybe :: TestTree
testMaybe = haskellReferenceTest defSpec
    { testName = "maybe"
    , source = \(nothingCase, maybeInt :: Maybe Int, offset) -> mconcat
        [ toStg "nothingCase" nothingCase
        , toStg "maybeInt" maybeInt
        , toStg "offset" offset
        , toStg "expectedResult" (maybe nothingCase (+offset) maybeInt)
        , Stg.maybe
        , Stg.add
        , Stg.eq_Int
        , [stg|
            main = \ =>
                letrec
                    addOffset = \x -> add x offset;
                    result = \(addOffset) -> maybe nothingCase addOffset maybeInt
                in case eq_Int result expectedResult of
                    True  -> Success;
                    wrong -> TestFail wrong
        |]]}
