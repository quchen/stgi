{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Temporary module to extract detail info message generation from "Evaluate".
-- Should be replaced by a proper ADT with prettyprinting.
module Stg.Machine.InfoDetails where



import qualified Data.Map                     as M
import           Data.Monoid                  hiding (Alt)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine.Types
import           Stg.Util



appF :: Var -> [Atom] -> InfoDetail
appF val [] = InfoDetail ["Inspect value " <> prettyprint val]
appF function args = InfoDetail
    [ (prettyprint . hsep)
        [ "Apply function"
        , pretty function
        , "to arguments"
        , commaSep (map pretty args) ]]



unusedLocals :: [Var] -> Locals -> InfoDetail
unusedLocals vars (Locals locals) = InfoDetail (
    let used   = M.fromList (map (,()) vars)
        unused = locals `M.difference` used
        prettyDiscardedBind var val = [pretty var <+> lparen <> pretty val <> rparen]
    in if M.null unused
        then []
        else [ prettyprint (
                "Unused local variables discarded:"
                <+> commaSep (M.foldMapWithKey prettyDiscardedBind unused) )])



enterNonUpdatable :: MemAddr -> InfoDetail
enterNonUpdatable addr = InfoDetail ["Enter closure at " <> prettyprint addr]



evalLet :: [Var] -> [MemAddr] -> InfoDetail
evalLet vars addrs = (InfoDetail . map prettyprint)
    [ hsep
        [ "Local environment extended by"
        , commaSep (foldMap (\var -> [pretty var]) vars) ]
    , hsep
        [ "Allocate new closures at"
        , commaSep (foldMap (\addr -> [pretty addr]) addrs)
        , "on the heap" ]]



evalCase :: InfoDetail
evalCase = InfoDetail [ "Push the alternatives and the local environment on the return stack" ]



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

garbageCollected :: Foldable f => f MemAddr -> InfoDetail
garbageCollected addrs = InfoDetail ["Removed addresses: " <> prettyAddrs addrs]
  where
    prettyAddrs = prettyprint . commaSep . foldMap (\addr -> [pretty addr])
