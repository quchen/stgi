{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extract Haskell values from running STG programs.
module Stg.Marshal.FromStg (
    FromStg(..),
    FromStgError(..),
) where



import Data.Bifunctor

import           Stg.Language
import qualified Stg.Machine.Env   as Env
import qualified Stg.Machine.Heap  as H
import           Stg.Machine.Types
import           Stg.Util



-- | Look up the value of a global variable.
--
-- Instances of this class should have a corresponding 'ToStg' instance to
-- inject a value into the program, with the two being inverse to each other (up
-- to forcing the generated thunks).
class FromStg value where

    -- | Retrieve the value of a global variable.
    fromStg
        :: StgState
        -> Var -- ^ Name of the global, e.g. @main@
        -> Either FromStgError value
    fromStg stgState = globalVal stgState (\case
        PrimInt{} -> Left TypeMismatch
        Addr addr -> fromStgAddr stgState addr )

    -- | Retrieve the value of a heap address.
    fromStgAddr
        :: StgState
        -> MemAddr
        -> Either FromStgError value

    -- | Used only for looking up primitive integers.
    fromStgPrim
        :: Integer
        -> Either FromStgError value
    fromStgPrim _ = Left TypeMismatch

    {-# MINIMAL fromStgAddr #-}

data FromStgError =
      TypeMismatch        -- ^ e.g. asking for an @Int#@ at an address
                          --   that contains a @Cons@
    | IsWrongLambdaType LambdaType -- ^ Tried retrieving a non-constructor
    | IsBlackhole         -- ^ Tried retrieving a black hole
    | BadArity         -- ^ e.g. @Cons x y z@
    | NotFound NotInScope -- ^ An unsuccessful variable lookup
    | AddrNotOnHeap
    | NoConstructorMatch  -- ^ None of the given alternatives matched the given
                          -- constructor, e.g. when trying to receive a 'Left'
                          -- as a 'Just'
    deriving (Eq, Ord, Show)

-- | Look up the global of a variable and handle the result.
--
-- Slighly bent version of 'Env.globalVal' to fit the types in this module
-- better.
globalVal
    :: FromStg value
    => StgState
    -> (Value -> Either FromStgError value) -- ^ What to do with the value if found
    -> Var                                  -- ^ Name of the global value to inspect
    -> Either FromStgError value
globalVal stgState f var = case Env.globalVal (stgGlobals stgState) (AtomVar var) of
    Failure _ -> Left (NotFound (NotInScope [var]))
    Success v -> f v

-- | Create a local environment.
--
-- Slighly bent version of 'Env.makeLocals' to fit the types in this module
-- better.
makeLocals :: [Var] -> [Value] -> Locals
makeLocals freeVars freeVals = Env.makeLocals (zipWith Mapping freeVars freeVals)

-- | Look up the value of an 'Atom' in a state, given a local environment.
atomVal
    :: FromStg value
    => StgState
    -> Locals
    -> Atom
    -> Either FromStgError value
atomVal stgState locals var = case Env.val locals (stgGlobals stgState) var of
    Failure notInScope -> Left (NotFound notInScope)
    Success (Addr addr) -> fromStgAddr stgState addr
    Success (PrimInt i)  -> fromStgPrim i

-- | Inspect whether a closure at a certain memory address matches the desired
-- criteria.
inspect
    :: FromStg value
    => StgState
    -> (Closure -> [Either (Maybe FromStgError) value])
        -- ^ List of possible matches, e.g. Nil and Cons in the list case.
        -- See e.g. 'matchCon2' in order to implement these matchers.
    -> MemAddr
    -> Either FromStgError value
inspect stgState inspectClosure addr = case H.lookup addr (stgHeap stgState) of
    Nothing -> Left AddrNotOnHeap
    Just heapObject -> case heapObject of
        Blackhole{} -> Left IsBlackhole
        HClosure closure -> firstMatch (inspectClosure closure)

  where
    firstMatch :: [Either (Maybe FromStgError) b] -> Either FromStgError b
    firstMatch (Right r : _)         = Right r
    firstMatch (Left Nothing : rest) = firstMatch rest
    firstMatch (Left (Just err) : _) = Left err
    firstMatch []                    = Left NoConstructorMatch
    firstMatch _ghc7_10_3 = error "Default to silence GHC's broken exhaustiveness checker"

instance FromStg () where
    fromStgAddr stgState = inspect stgState (\closure ->
        [matchCon0 "Unit" closure])

instance FromStg Bool where
    fromStgAddr stgState = inspect stgState (\closure ->
        [ True  <$ matchCon0 "True"  closure
        , False <$ matchCon0 "False" closure ])

-- | Boxed (@Int\# 1\#@) or unboxed (@1#@)
instance FromStg Integer where
    fromStg stgState var = case Env.globalVal (stgGlobals stgState) (AtomVar var) of
        Failure _ -> Left (NotFound (NotInScope [var]))
        Success val -> case val of
            PrimInt i -> Right i
            Addr addr -> fromStgAddr stgState addr
    fromStgAddr stgState = inspect stgState (\closure ->
        [ matchCon1 "Int#" closure >>= \(x, locals) ->
            liftToMatcher (atomVal stgState locals x) ])
    fromStgPrim i = Right i

instance (FromStg a, FromStg b) => FromStg (a,b) where
    fromStgAddr stgState = inspect stgState (\closure ->
        [ matchCon2 "Pair" closure >>= \((x,y), locals) ->
            (,) <$> liftToMatcher (atomVal stgState locals x)
                <*> liftToMatcher (atomVal stgState locals y) ])

instance (FromStg a, FromStg b, FromStg c) => FromStg (a,b,c) where
    fromStgAddr stgState = inspect stgState (\closure ->
        [ matchCon3 "Triple" closure >>= \((x,y,z), locals) ->
            (,,) <$> liftToMatcher (atomVal stgState locals x)
                 <*> liftToMatcher (atomVal stgState locals y)
                 <*> liftToMatcher (atomVal stgState locals z) ])

instance (FromStg a, FromStg b, FromStg c, FromStg d) => FromStg (a,b,c,d) where
    fromStgAddr stgState = inspect stgState (\closure ->
        [ matchCon4 "Quadruple" closure >>= \((x,y,z,w), locals) ->
            (,,,) <$> liftToMatcher (atomVal stgState locals x)
                  <*> liftToMatcher (atomVal stgState locals y)
                  <*> liftToMatcher (atomVal stgState locals z)
                  <*> liftToMatcher (atomVal stgState locals w) ])

instance (FromStg a, FromStg b, FromStg c, FromStg d, FromStg e) => FromStg (a,b,c,d,e) where
    fromStgAddr stgState = inspect stgState (\closure ->
        [ matchCon5 "Quintuple" closure >>= \((x,y,z,w,v), locals) ->
            (,,,,) <$> liftToMatcher (atomVal stgState locals x)
                   <*> liftToMatcher (atomVal stgState locals y)
                   <*> liftToMatcher (atomVal stgState locals z)
                   <*> liftToMatcher (atomVal stgState locals w)
                   <*> liftToMatcher (atomVal stgState locals v) ])

instance FromStg a => FromStg (Maybe a) where
    fromStgAddr stgState = inspect stgState (\closure ->
        [ Nothing <$ matchCon0 "Nothing" closure
        , matchCon1 "Just" closure >>= \(arg, locals) ->
            Just <$> liftToMatcher (atomVal stgState locals arg) ])

instance (FromStg a, FromStg b) => FromStg (Either a b) where
    fromStgAddr stgState = inspect stgState (\closure ->
        [ matchCon1 "Left" closure >>= \(arg, locals) ->
            Left  <$> liftToMatcher (atomVal stgState locals arg)
        , matchCon1 "Right" closure >>= \(arg, locals) ->
            Right <$> liftToMatcher (atomVal stgState locals arg) ])

instance FromStg a => FromStg [a] where
    fromStgAddr stgState = inspect stgState (\closure ->
        [ [] <$ matchCon0 "Nil" closure
        , matchCon2 "Cons" closure >>= \((x,xs), locals) ->
             (:) <$> liftToMatcher (atomVal stgState locals x)
                 <*> liftToMatcher (atomVal stgState locals xs) ])

-- | Lift an errable value into a context where the specific error is not
-- necessarily present.
liftToMatcher :: Either e a -> Either (Maybe e) a
liftToMatcher = first Just

-- | Like 'matchCon2', but for nullary 'Constr'uctors.
matchCon0 :: Constr -> Closure -> Either (Maybe FromStgError) ()
matchCon0 _ (Closure lambdaForm _)
    | classify lambdaForm == LambdaThunk = Left (Just (IsWrongLambdaType LambdaThunk))
    | classify lambdaForm == LambdaFun   = Left (Just (IsWrongLambdaType LambdaFun))
matchCon0 wantedCon (Closure (LambdaForm _ _ _ (AppC actualCon args)) _)
    | wantedCon == actualCon = case args of
        []  -> Right ()
        _xs -> Left (Just BadArity)
matchCon0 _ _ = Left Nothing

-- | Like 'matchCon2', but for unary 'Constr'uctors.
matchCon1 :: Constr -> Closure -> Either (Maybe FromStgError) (Atom, Locals)
matchCon1 _ (Closure lambdaForm _)
    | classify lambdaForm == LambdaThunk = Left (Just (IsWrongLambdaType LambdaThunk))
    | classify lambdaForm == LambdaFun   = Left (Just (IsWrongLambdaType LambdaFun))
matchCon1 wantedCon (Closure (LambdaForm freeVars _ _ (AppC actualCon args)) freeVals)
    | wantedCon == actualCon = case args of
        [x] -> Right (x, makeLocals freeVars freeVals)
        _xs -> Left (Just BadArity)
matchCon1 _ _ = Left Nothing

-- | Match a 'Closure' for a binary 'Constr'uctor.
--
-- * If the constructor matches, return its arguments, and the local environment
--   stored in the closure.
-- * If the constructor does not match, return 'Nothing' as error, indicating
--   to the caller that the next matcher should be tried.
-- * If the constructor fails due to a non-recoverable error, such as wrong
--   arity, abort with the corresponding error.
matchCon2 :: Constr -> Closure -> Either (Maybe FromStgError) ((Atom, Atom), Locals)
matchCon2 _ (Closure lambdaForm _)
    | classify lambdaForm == LambdaThunk = Left (Just (IsWrongLambdaType LambdaThunk))
    | classify lambdaForm == LambdaFun   = Left (Just (IsWrongLambdaType LambdaFun))
matchCon2 wantedCon (Closure (LambdaForm freeVars _ _ (AppC actualCon args)) freeVals)
    | wantedCon == actualCon = case args of
        [x,y] -> Right ((x,y), makeLocals freeVars freeVals)
        _xs   -> Left (Just BadArity)
matchCon2 _ _ = Left Nothing

-- | Like 'matchCon2', but for ternary 'Constr'uctors.
matchCon3 :: Constr -> Closure -> Either (Maybe FromStgError) ((Atom, Atom, Atom), Locals)
matchCon3 _ (Closure lambdaForm _)
    | classify lambdaForm == LambdaThunk = Left (Just (IsWrongLambdaType LambdaThunk))
    | classify lambdaForm == LambdaFun   = Left (Just (IsWrongLambdaType LambdaFun))
matchCon3 wantedCon (Closure (LambdaForm freeVars _ _ (AppC actualCon args)) freeVals)
    | wantedCon == actualCon = case args of
        [x,y,z] -> Right ((x,y,z), makeLocals freeVars freeVals)
        _xs     -> Left (Just BadArity)
matchCon3 _ _ = Left Nothing

-- | Like 'matchCon2', but for 4-ary 'Constr'uctors.
matchCon4 :: Constr -> Closure -> Either (Maybe FromStgError) ((Atom, Atom, Atom, Atom), Locals)
matchCon4 _ (Closure lambdaForm _)
    | classify lambdaForm == LambdaThunk = Left (Just (IsWrongLambdaType LambdaThunk))
    | classify lambdaForm == LambdaFun   = Left (Just (IsWrongLambdaType LambdaFun))
matchCon4 wantedCon (Closure (LambdaForm freeVars _ _ (AppC actualCon args)) freeVals)
    | wantedCon == actualCon = case args of
        [x,y,z,w] -> Right ((x,y,z,w), makeLocals freeVars freeVals)
        _xs       -> Left (Just BadArity)
matchCon4 _ _ = Left Nothing

-- | Like 'matchCon2', but for 5-ary 'Constr'uctors.
matchCon5 :: Constr -> Closure -> Either (Maybe FromStgError) ((Atom, Atom, Atom, Atom, Atom), Locals)
matchCon5 _ (Closure lambdaForm _)
    | classify lambdaForm == LambdaThunk = Left (Just (IsWrongLambdaType LambdaThunk))
    | classify lambdaForm == LambdaFun   = Left (Just (IsWrongLambdaType LambdaFun))
matchCon5 wantedCon (Closure (LambdaForm freeVars _ _ (AppC actualCon args)) freeVals)
    | wantedCon == actualCon = case args of
        [x,y,z,w,v] -> Right ((x,y,z,w,v), makeLocals freeVars freeVals)
        _xs         -> Left (Just BadArity)
matchCon5 _ _ = Left Nothing
