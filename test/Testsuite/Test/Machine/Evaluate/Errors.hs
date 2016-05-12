{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.Evaluate.Errors (tests) where



import Test.Tasty

import Stg.Machine
import Stg.Machine.Types
import Stg.Parser

import Test.Machine.Evaluate.TestTemplates.MachineState
import Test.Orphans                                     ()



tests :: TestTree
tests = testGroup "Error conditions"
    [ loopEnterBlackHole ]

defSpec :: MachineStateTestSpec
defSpec = MachineStateTestSpec
    { testName             = "Default error condition test template"
    , successPredicate     = const False
    , forbiddenState       = const False
    , someStateSatisfies   = const True
    , source               = [stg| main = () \n () -> main () |]
    , maxSteps             = 1024
    , performGc            = PerformGc (const True)
    , failWithInfo         = False }

loopEnterBlackHole :: TestTree
loopEnterBlackHole = machineStateTest defSpec
    { testName = "Loop on entering black hole"
    , source = [stg| main = () \u () -> main () |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError EnterBlackhole) _ -> True
        _otherwise -> False }
