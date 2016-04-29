{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

-- | Temporary module to extract detail info message generation from "Evaluate".
-- Should be replaced by a proper ADT with prettyprinting.
module Stg.Machine.InfoDetails where



import qualified Data.Map                     as M
import           Data.Monoid                  hiding (Alt)
import qualified Data.Text                    as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine.Types



appF :: Var -> [Atom] -> InfoDetail
appF function args =
    InfoDetail
         [ T.unwords
            [ "Apply function"
            , prettyprint function
            , case args of
                [] -> "without arguments"
                _  -> T.unwords
                    [ " to arguments "
                    , T.intercalate ", " (foldMap (\arg -> [prettyprint arg]) args) ]]]


unusedLocals :: [Var] -> Locals -> InfoDetail
unusedLocals vars locals = InfoDetail (
    let Locals loc = locals
        used = M.fromList (map (,()) vars)
        discarded = loc `M.difference` used
        prettyDiscardedBind var val = [prettyprint (pretty var <+> lparen <> pretty val <> rparen)]
    in if M.null discarded
        then []
        else [ "Unused local variables discarded: "
            <> T.intercalate ", " (M.foldMapWithKey prettyDiscardedBind discarded) ])



enterNonUpdatable :: MemAddr -> InfoDetail
enterNonUpdatable addr = InfoDetail ["Enter closure at " <> prettyprint addr]



evalLet :: [Var] -> [MemAddr] -> InfoDetail
evalLet vars addrs = InfoDetail
    [ T.unwords
        [ "Local environment extended by"
        , T.intercalate ", " (foldMap (\var -> [prettyprint var]) vars) ]
    , T.unwords
        [ "Allocate new closures at"
        , T.intercalate ", " (foldMap (\addr -> [prettyprint addr]) addrs)
        , "on the heap" ]]



evalCase :: InfoDetail
evalCase = InfoDetail [ "Push the alternatives and the local environment on the update stack" ]



enterUpdatable :: MemAddr -> InfoDetail
enterUpdatable addr = InfoDetail
    [ "Push a new update frame with the entered address " <> prettyprint addr
    , "Save current argument and return stacks on that update frame"
    , "Argument and return stacks are now empty"  ]



conUpdate :: Constr -> MemAddr -> InfoDetail
conUpdate con addrU = InfoDetail
    [ "Trying to return " <> prettyprint con <> " without anything on argument/return stacks"
    , "Update closure at " <> prettyprint addrU <> " with returned constructor"
    , "Restore argument/return stacks from the update frame" ]



returnIntCannotUpdate :: InfoDetail
returnIntCannotUpdate = InfoDetail
    ["No closure has primitive type, so we cannot update one with a primitive int"]



stacksNotEmpty :: InfoDetail
stacksNotEmpty = InfoDetail
    [ "Stacks are not empty; the program terminated unexpectedly."
    , "The lack of a better description is a bug in the STG evaluator."
    , "Please report this to the project maintainers!" ]
