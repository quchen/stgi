{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Marshal (tests) where



import           Data.Proxy
import           Data.Text  (Text)
import qualified Data.Text  as T

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Machine
import Stg.Marshal
import Stg.Parser.QuasiQuoter

import Test.Orphans          ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Marshalling"
    [ boolRoundtrip
    , integerRoundtrip
    , unitRoundtrip
    , listRoundtrip
    , nestedListRoundtrip
    , maybeRoundtrip
    , nestedMaybeRoundtrip
    , eitherRoundtrip
    , tuple2Roundtrip
    , tuple3Roundtrip
    , tuple4Roundtrip
    , tuple5Roundtrip ]

-- | Specifies a test that is based on the reduction of a closure.
data RoundtripSpec a = RoundtripSpec
    { testName :: Text

    , force    :: Program
        -- ^ @force@ function to evaluate the payload to WHNF

    , testType :: Proxy a
        -- ^ Pin down the type of @a@
    }

roundTripTest
    :: forall a. (Arbitrary a, ToStg a, FromStg a, Show a, Eq a)
    => RoundtripSpec a
    -> TestTree
roundTripTest spec = testProperty (T.unpack (testName spec)) test
  where
    test :: (Arbitrary a, ToStg a, FromStg a, Show a, Eq a) => a -> Property
    test payload = counterexample (T.unpack (prettyprint source)) (expected === Right payload)
      where
        source = mconcat
            [ force spec
            , toStg "payload" payload
            , [stg|
                main = \ => force payload |]]
        prog = initialState "main" source
        states = evalsUntil
            (RunForMaxSteps 1000)
            (HaltIf (const False))
            (PerformGc (const Nothing))
            prog
        finalState = last states
        expected = fromStg finalState "main"


boolRoundtrip :: TestTree
boolRoundtrip = roundTripTest RoundtripSpec
    { testName = "Bool"
    , testType = Proxy :: Proxy Bool
    , force = [stg| force = \x -> case x of r -> r |] }

integerRoundtrip :: TestTree
integerRoundtrip = roundTripTest RoundtripSpec
    { testName = "Integer"
    , testType = Proxy :: Proxy Integer
    , force = [stg| force = \x -> case x of r -> r |] }

unitRoundtrip :: TestTree
unitRoundtrip = roundTripTest RoundtripSpec
    { testName = "Unit"
    , testType = Proxy :: Proxy ()
    , force = [stg| force = \x -> case x of r -> r |] }

listRoundtrip :: TestTree
listRoundtrip = roundTripTest RoundtripSpec
    { testName = "List"
    , testType = Proxy :: Proxy [Integer]
    , force = [stg|
        force = \xs -> case forceList xs of _ -> xs;
        forceList = \xs -> case xs of
            Nil -> Done;
            Cons y ys -> case y of
                _ -> forceList ys;
            _ -> Error
        |] }

nestedListRoundtrip :: TestTree
nestedListRoundtrip = roundTripTest RoundtripSpec
    { testName = "Nested list"
    , testType = Proxy :: Proxy [[Integer]]
    , force = [stg|
        force = \x -> case forceUntyped x of _ -> x;

        forceUntyped = \x -> case x of
            Nil -> Done;
            Int# _ -> Done;
            Cons ys yss -> case forceUntyped ys of
                _ -> forceUntyped yss;
            _ -> Error
        |] }

maybeRoundtrip :: TestTree
maybeRoundtrip = roundTripTest RoundtripSpec
    { testName = "Maybe"
    , testType = Proxy :: Proxy (Maybe Integer)
    , force = [stg| force = \x -> case x of r -> r |] }

nestedMaybeRoundtrip :: TestTree
nestedMaybeRoundtrip = roundTripTest RoundtripSpec
    { testName = "Maybe"
    , testType = Proxy :: Proxy (Maybe (Maybe Integer))
    , force = [stg|
        force = \x -> case forceUntyped x of _ -> x;

        forceUntyped = \x -> case x of
            Nothing -> Done;
            Int# _ -> Done;
            Just y -> forceUntyped y;
            _ -> Error |] }

eitherRoundtrip :: TestTree
eitherRoundtrip = roundTripTest RoundtripSpec
    { testName = "Maybe"
    , testType = Proxy :: Proxy (Either Integer Bool)
    , force = [stg| force = \x -> case x of r -> r |] }

tuple2Roundtrip :: TestTree
tuple2Roundtrip = roundTripTest RoundtripSpec
    { testName = "2-tuple"
    , testType = Proxy :: Proxy (Integer, Integer)
    , force = [stg| force = \x -> case x of r -> r |] }

tuple3Roundtrip :: TestTree
tuple3Roundtrip = roundTripTest RoundtripSpec
    { testName = "3-tuple"
    , testType = Proxy :: Proxy (Integer, Integer, Integer)
    , force = [stg| force = \x -> case x of r -> r |] }

tuple4Roundtrip :: TestTree
tuple4Roundtrip = roundTripTest RoundtripSpec
    { testName = "4-tuple"
    , testType = Proxy :: Proxy (Integer, Integer, Integer, Integer)
    , force = [stg| force = \x -> case x of r -> r |] }

tuple5Roundtrip :: TestTree
tuple5Roundtrip = roundTripTest RoundtripSpec
    { testName = "5-tuple"
    , testType = Proxy :: Proxy (Integer, Integer, Integer, Integer, Integer)
    , force = [stg| force = \x -> case x of r -> r |] }
