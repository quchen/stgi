{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Maybe (tests) where



import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.MarshalledValue
import Test.Orphans                                        ()
import Test.Tasty



tests :: TestTree
tests = testGroup "Maybe"
    [ testMaybe ]

testMaybe :: TestTree
testMaybe = marshalledValueTest defSpec
    { testName = "maybe"
    , sourceSpec = \(nothingCase, maybeInt :: Maybe Integer, offset) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = maybe nothingCase (+offset) maybeInt
        , source = mconcat
            [ toStg "nothingCase" nothingCase
            , toStg "maybeInt" maybeInt
            , toStg "offset" offset
            , Stg.maybe
            , Stg.add
            , [stg|
                main = \ =>
                    let addOffset = \x -> add x offset
                    in maybe nothingCase addOffset maybeInt |]]}}
