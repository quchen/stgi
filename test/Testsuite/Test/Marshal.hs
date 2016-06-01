{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Marshal (tests) where



import           Data.Proxy
import           Data.Text  (Text)
import qualified Data.Text  as T

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
    , testGroup "Tuples"
        [tuple2Roundtrip
        , tuple3Roundtrip
        , tuple4Roundtrip
        , tuple5Roundtrip ]
    , crazyRoundtrip
    ]

-- | Specifies a test that is based on the reduction of a closure.
data RoundtripSpec a = RoundtripSpec
    { testName :: Text

    , testType :: Proxy a
        -- ^ Pin down the type of @a@

    , maxSteps :: Integer
    }

defSpec :: RoundtripSpec a
defSpec = RoundtripSpec
    { testName = "Default test spec"
    , testType = Proxy
    , maxSteps = 10e3}

roundTripTest
    :: forall a. (Arbitrary a, ToStg a, FromStg a, Show a, Eq a)
    => RoundtripSpec a
    -> TestTree
roundTripTest spec = testProperty (T.unpack (testName spec)) test
  where
    test :: (Arbitrary a, ToStg a, FromStg a, Show a, Eq a) => a -> Property
    test payload = counterexample (T.unpack (prettyprint finalState))
                                  (expected === Right payload)
      where
        source = mconcat
            [ toStg "payload" payload
            , [stg|
                main = \ => case forceUntyped payload of _ -> Done;

                forceUntyped = \value -> case value of
                    Unit -> Done;

                    Nothing -> Done;
                    Just x -> forceUntyped x;

                    True -> Done;
                    False -> Done;

                    Int# _ -> Done;

                    Left l -> forceUntyped l;
                    Right r -> forceUntyped r;

                    Pair x y -> case forceUntyped x of
                        _ -> forceUntyped y;
                    Triple x y z -> case forceUntyped x of
                        _ -> case forceUntyped y of
                            _ -> forceUntyped z;

                    Nil -> Done;
                    Cons x xs -> case forceUntyped x of
                        _ -> forceUntyped xs;

                    _ -> Error
                 |]]
        prog = initialState "main" source
        states = evalsUntil
            (RunForMaxSteps (maxSteps spec))
            (HaltIf (const False))
            (PerformGc (const Nothing))
            prog
        finalState = garbageCollect triStateTracing (last states)
        expected = fromStg finalState "payload"


boolRoundtrip :: TestTree
boolRoundtrip = roundTripTest defSpec
    { testName = "Bool"
    , testType = Proxy :: Proxy Bool }

integerRoundtrip :: TestTree
integerRoundtrip = roundTripTest defSpec
    { testName = "Integer"
    , testType = Proxy :: Proxy Integer }

unitRoundtrip :: TestTree
unitRoundtrip = roundTripTest defSpec
    { testName = "Unit"
    , testType = Proxy :: Proxy () }

listRoundtrip :: TestTree
listRoundtrip = roundTripTest defSpec
    { testName = "List"
    , testType = Proxy :: Proxy [Integer] }

nestedListRoundtrip :: TestTree
nestedListRoundtrip = roundTripTest defSpec
    { testName = "Nested list"
    , testType = Proxy :: Proxy [[Integer]] }

maybeRoundtrip :: TestTree
maybeRoundtrip = roundTripTest defSpec
    { testName = "Maybe"
    , testType = Proxy :: Proxy (Maybe Integer) }

nestedMaybeRoundtrip :: TestTree
nestedMaybeRoundtrip = roundTripTest defSpec
    { testName = "Maybe"
    , testType = Proxy :: Proxy (Maybe (Maybe Integer)) }

eitherRoundtrip :: TestTree
eitherRoundtrip = roundTripTest defSpec
    { testName = "Maybe"
    , testType = Proxy :: Proxy (Either Integer Bool) }

tuple2Roundtrip :: TestTree
tuple2Roundtrip = roundTripTest defSpec
    { testName = "2-tuple"
    , testType = Proxy :: Proxy (Integer, Integer) }

tuple3Roundtrip :: TestTree
tuple3Roundtrip = roundTripTest defSpec
    { testName = "3-tuple"
    , testType = Proxy :: Proxy (Integer, Integer, Integer) }

tuple4Roundtrip :: TestTree
tuple4Roundtrip = roundTripTest defSpec
    { testName = "4-tuple"
    , testType = Proxy :: Proxy (Integer, Integer, Integer, Integer) }

tuple5Roundtrip :: TestTree
tuple5Roundtrip = roundTripTest defSpec
    { testName = "5-tuple"
    , testType = Proxy :: Proxy (Integer, Integer, Integer, Integer, Integer) }

crazyRoundtrip :: TestTree
crazyRoundtrip = roundTripTest defSpec
    { testName = "Crazy giant type"
    , testType = Proxy :: Proxy [Either ([[[Integer]]], [Maybe ()]) (Maybe [([Bool], Integer, [Bool])])]
    , maxSteps = 10e5 }
