{-# LANGUAGE QuasiQuotes #-}

module Stg.Language.Prelude.Number (
    numbers,
    add,
    leq,
    gt,
    eq,
) where



import           Prelude      ()

import           Stg.Language (Program)
import           Stg.Parser



numbers, add :: Program
leq, gt, eq :: Program



-- | Various common numbers.
--
-- @
-- minusOne = -1
-- zero     =  0
-- one      =  1
-- two      =  2
-- three    =  3
-- ten      = 10
-- @
numbers = [stg|
    minusOne = () \n () -> Int# (-1#);
    zero     = () \n () -> Int# (0#);
    one      = () \n () -> Int# (1#);
    two      = () \n () -> Int# (2#);
    three    = () \n () -> Int# (3#);
    ten      = () \n () -> Int# (10#) |]

-- | Integer addition.
--
-- @
-- add : Int -> Int -> Int
-- @
add = [stg|
    add = () \n (x,y) -> case x () of
        Int# (x') -> case y () of
            Int# (y') -> case +# x' y' of
                1# -> Int# (1#); -- FIXME type hint
                v -> Int# (v);
            default -> Error_add ();
        default -> Error_add () |]

-- | @<=@
--
-- @
-- leq : Int -> Int -> Int
-- @
leq = [stg|
    leq = () \n (x,y) -> case x () of
        Int# (x') -> case y () of
            Int# (y') -> case <=# x' y' of
                1# -> Int# (1#); -- FIXME type hint
                v  -> Int# (v);
            default -> Error_leq ();
        default -> Error_leq () |]


-- | @>@
--
-- @
-- gt : Int -> Int -> Int
-- @
gt = [stg|
    gt = () \n (x,y) -> case x () of
        Int# (x') -> case y () of
            Int# (y') -> case ># x' y' of
                1# -> Int# (1#); -- FIXME type hint
                v  -> Int# (v);
            default -> Error_gt ();
        default -> Error_gt () |]



-- | @==@
--
-- @
-- eq : Int -> Int -> Int
-- @
eq = [stg|
    eq = () \n (x,y) -> case x () of
        Int# (x') -> case y () of
            Int# (y') -> case ==# x' y' of
                1# -> Int# (1#); -- FIXME type hint
                v  -> Int# (v);
            v -> Error_eq (v);
        v -> Error_gt (v) |]
