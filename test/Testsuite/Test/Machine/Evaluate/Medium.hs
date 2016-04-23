{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Tests of medium size. These tests will be run with garbage collection
-- enabled, and should have the scope of small functions a Haskell beginner
-- might play around with.
module Test.Machine.Evaluate.Medium (tests) where



import           Data.Monoid
import           Test.Tasty

import qualified Stg.Language.Prelude                       as Stg
import           Stg.Machine
import           Stg.Parser

import           Test.Machine.Evaluate.ClosureReductionTest
import           Test.Orphans                               ()



tests :: TestTree
tests = testGroup "Medium-sized, with GC"
    [ program_add3
    , program_foldrSum
    , program_takeRepeat
    , program_map ]

defSpec :: ClosureReductionSpec
defSpec = ClosureReductionSpec
    { testName         = "Default medium closure reduction test template"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source           = [stg| main = () \n () -> Success () |]
    , maxSteps         = 256
    , performGc        = PerformGc (const True) }

program_add3 :: TestTree
program_add3 = closureReductionTest defSpec
    { testName = "add3(x,y,z) = x+y+z"
    , source = [stg|
        add3 = () \n (x,y,z) -> case x () of
            Int# (i) -> case y () of
                Int# (j) -> case +# i j of
                    12345# -> 1#; -- type hInt# FIXME
                    ij -> case z () of
                        Int# (k) -> case +# ij k of
                            12345# -> 1#; -- type hInt# FIXME
                            ijk -> Int# (ijk);
                        default -> Error ()
                default -> Error ()
            default -> Error ();

        one   = () \n () -> Int# (1#);
        two   = () \n () -> Int# (2#);
        three = () \n () -> Int# (3#);
        main = () \u () -> case add3 (one, two, three) of
            Int# (i) -> case i () of
                6# -> Success ();
                wrongResult -> TestFail (wrongResult);
            default -> Error ()
        |] }

program_foldrSum :: TestTree
program_foldrSum = closureReductionTest defSpec
    { testName = "Sum of list via foldr"
    , source = [stgProgram|
        foldr = () \n (f, z, xs) -> case xs () of
            Nil () -> z ();
            Cons (y,ys) ->
                let rest = (f,z,ys) \n () -> foldr (f,z,ys)
                in f (y, rest);
            default -> Error ();

        add2 = () \n (x,y) -> case x () of
            Int# (x') -> case y () of
                Int# (y') -> case +# x' y' of
                    1# -> Int# (1#); -- FIXME type hint
                    v -> Int# (v);
                default -> Error ();
            default -> Error ();

        zero = () \n () -> Int# (0#);

        sum = () \n (xs) -> foldr (add2, zero, xs);

        list = () \u () ->
            letrec one   = () \n () -> Int# (1#);
                   two   = () \n () -> Int# (2#);
                   three = () \n () -> Int# (3#);
                   nil      = () \n () -> Nil ();
                   list3    = (three, nil)     \n () -> Cons (three, nil);
                   list23   = (two, list3)     \n () -> Cons (two,   list3);
                   list123  = (one, list23)    \n () -> Cons (one,   list23);
                   list3123 = (three, list123) \n () -> Cons (three, list123)
            in list3123 ();

        main = () \u () -> case sum (list) of
            Int# (i) -> case i () of
                9# -> Success ();
                wrongResult -> TestFail (wrongResult);
            default -> Error ()
        |] }

program_takeRepeat :: TestTree
program_takeRepeat = closureReductionTest defSpec
    { testName = "take 2 (repeat ())"
    , source = Stg.numbers
            <> Stg.take
            <> Stg.repeat
            <> Stg.foldr
            <> Stg.seq
            <> [stgProgram|

        consBang = () \n (x,xs) -> case xs () of v -> Cons (x, v);
        nil = () \n () -> Nil ();
        forceSpine = () \n (xs) -> foldr (consBang, nil, xs);

        twoUnits = () \u () ->
            letrec  repeated = (unit) \u () -> repeat (unit);
                    unit = () \n () -> Unit ();
                    take2 = (repeated) \u () -> take (two, repeated)
            in      forceSpine (take2);

        main = () \u () -> case twoUnits () of
            Cons (x,xs) -> case xs () of
                Cons (y,ys) -> case ys () of
                    Nil () -> Success ();
                    default -> TestFailure ();
                default -> TestFailure ();
            default -> TestFailure ()
        |] }

program_map :: TestTree
program_map = closureReductionTest defSpec
    { testName = "map (+1) [1,2,3]"
    , source = Stg.numbers
            <> Stg.add
            <> Stg.map
            <> [stgProgram|

        list = () \u () -> letrec nil = () \n () -> Nil ();
                                  list3 = (nil) \u () -> Cons (three, nil);
                                  list23 = (list3) \u () -> Cons (two, list3);
                                  list123 = (list23) \u () -> Cons (one, list23)
                           in list123 ();

        plusOne = () \n (n) -> add (n, one);

        main = () \u () -> case map (plusOne, list) of
            Cons (x,xs) -> case xs () of
                Cons (y, ys) -> case ys () of
                    Cons (z, zs) -> case zs () of
                        Nil () -> case x () of
                            Int# (xPrim) -> case xPrim () of
                                2# -> case y () of
                                    Int# (yPrim) -> case yPrim () of
                                        3# -> case z () of
                                            Int# (zPrim) -> case zPrim () of
                                                4# -> Success ()
                                                default -> Error_Third_item_should_be_four ()
                                            default -> Error_Third_item_should_be_a_number ()
                                        default -> Error_Second_item_should_be_three ()
                                    default -> Error_Second_item_should_be_a_number ()
                                default -> Error_first_item_should_be_two ()
                            default -> Error_first_item_should_be_a_number ()
                        default -> Error_List_has_too_many_items ()
                    default -> Error_List_has_only_two_items ()
                default -> Error_List_has_only_one_item ()
            default -> Error_List_is_empty_or_not_a_list ()

        |] }
