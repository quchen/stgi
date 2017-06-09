{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Aeson               hiding (Value)
import qualified Data.Aeson               as Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as BSL
import           Data.Foldable
import qualified Data.Map                 as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Builder   as LTB

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree

import           Data.Stack
import qualified Stg.ExamplePrograms as ExampleProgram
import           Stg.Language
import           Stg.Machine
import           Stg.Machine.Types



import Stg.Language.Prettyprint
    (AstAnn (..), PrettyStgi (..), StateAnn (..), StgiAnn (..))

prog :: Program
prog = ExampleProgram.fibonacciZipWith

main :: IO ()
main = do
    let allStates = evalsUntil
            (RunForMaxSteps 32)
            (HaltIf (const False))
            (PerformGc (const (Just triStateTracing)))
            (initialState "main" prog)

        allStatesJson = fmap toJson_StgState allStates

    BSL.putStr (encodePretty (toJSON (toList allStatesJson)))



toJson_StgState :: StgState -> Aeson.Value
toJson_StgState (StgState
    { stgCode    = xStgCode
    , stgStack   = xStgStack
    , stgHeap    = xStgHeap
    , stgGlobals = xStgGlobals
    , stgSteps   = xStgSteps
    , stgInfo    = xStgInfo })
  = object
        [ "code"    .= toJson_Code xStgCode
        , "stack"   .= toJson_Stack xStgStack
        , "heap"    .= toJson_Heap xStgHeap
        , "globals" .= toJson_Globals xStgGlobals
        , "step"    .= toJSON xStgSteps
        , "info"    .= toJson_Info xStgInfo
        ]

toJson_Code :: Code -> Aeson.Value
toJson_Code = \case
    Eval expr _locals   -> object [ "eval" .= toJson_Expr expr
                                  , "locals" .= notImplementedYet ]
    Enter addr          -> object [ "enter" .= toJson_Addr addr ]
    ReturnCon con _args -> object [ "ReturnCon" .= toJson_Constr con
                                  , "args" .= notImplementedYet ]
    ReturnInt i         -> object ["ReturnInt" .= toJSON i]

toJson_Constr :: Constr -> Aeson.Value
toJson_Constr (Constr con) = toJSON con

toJson_Stack :: Stack StackFrame -> Aeson.Value
toJson_Stack = toJSON . toList . fmap toJson_StackFrame

toJson_StackFrame :: StackFrame -> Aeson.Value
toJson_StackFrame = \case
    ArgumentFrame val -> object ["argument" .= toJson_Value val]
    ReturnFrame{}     -> notImplementedYet
    UpdateFrame addr  -> object ["update" .= toJson_Addr addr]

toJson_Heap :: Heap -> Aeson.Value
toJson_Heap (Heap h) = toJSON (do
    (addr, ho) <- M.toList h
    pure (object
        ["address" .= toJson_Addr addr
        , "heapObject" .= toJson_HeapObject ho ]))

toJson_HeapObject :: HeapObject -> Aeson.Value
toJson_HeapObject = \case
    Blackhole step -> object ["blackhole" .= toJSON step]
    HClosure closure -> object ["closure" .= toJson_Closure closure ]

toJson_Closure :: Closure -> Aeson.Value
toJson_Closure = toJSON . renderReactHtml . prettyStgi

toJson_Expr :: Expr -> Aeson.Value
toJson_Expr = toJSON . renderReactHtml . prettyStgi

toJson_Globals :: Globals -> Aeson.Value
toJson_Globals (Globals gs) = object
    [ var .= toJson_Value val | (Var var, val) <- M.toList gs ]

toJson_Value :: Value -> Aeson.Value
toJson_Value = \case
    Addr addr -> object [ "address" .= toJson_Addr addr ]
    PrimInt i -> object [ "prim-int" .= toJSON i ]

toJson_Addr :: MemAddr -> Aeson.Value
toJson_Addr (MemAddr addr) = toJSON addr

toJson_Info :: Info -> Aeson.Value
toJson_Info (Info short _details)
  = object
        [ "short" .= toJson_InfoShort short
        , "details" .= notImplementedYet
        ]

notImplementedYet :: Aeson.Value
notImplementedYet = toJSON ("Not implemented yet" :: Text)

toJson_InfoShort :: InfoShort -> Aeson.Value
toJson_InfoShort = \case
    NoRulesApply      -> "NoRulesApply"
    MaxStepsExceeded  -> "MaxStepsExceeded"
    HaltedByPredicate -> "HaltedByPredicate"
    StateError _      -> "StateError"
    StateInitial      -> "StateInitial"
    GarbageCollection -> "GarbageCollection"
    StateTransition Rule1_Eval_FunctionApplication       -> "1-eval-func"
    StateTransition Rule2_Enter_NonUpdatableClosure      -> "2-enter-nonupd"
    StateTransition (Rule3_Eval_Let NonRecursive)        -> "3-eval-let"
    StateTransition (Rule3_Eval_Let Recursive)           -> "3-eval-recrec"
    StateTransition Rule4_Eval_Case                      -> "4-eval-case"
    StateTransition Rule5_Eval_AppC                      -> "5-eval-app-con"
    StateTransition Rule6_ReturnCon_Match                -> "6-returncon-match"
    StateTransition Rule7_ReturnCon_DefUnbound           -> "7-returncon-defunbound"
    StateTransition Rule8_ReturnCon_DefBound             -> "8-returncon-defbound"
    StateTransition Rule9_Lit                            -> "9-eval-lit"
    StateTransition Rule10_LitApp                        -> "10-eval-litapp"
    StateTransition Rule11_ReturnInt_Match               -> "11-returnint-match"
    StateTransition Rule12_ReturnInt_DefBound            -> "12-returnint-defbound"
    StateTransition Rule13_ReturnInt_DefUnbound          -> "13-returnint-defunbound"
    StateTransition Rule14_Eval_AppP                     -> "14-eval-appprim"
    StateTransition Rule15_Enter_UpdatableClosure        -> "15-enter-upd"
    StateTransition Rule16_ReturnCon_Update              -> "16-returncon-upd"
    StateTransition Rule17_Enter_PartiallyAppliedUpdate  -> "17-enter-upd"
    StateTransition Rule17a_Enter_PartiallyAppliedUpdate -> "18-enter-pap"
    StateTransition Rule1819_Eval_Case_Primop_Shortcut   -> "19-eval-caseprim"

renderReactHtml :: Doc StgiAnn -> Text
renderReactHtml
    = LT.toStrict
    . LTB.toLazyText
    . toRawHtmlBuilder
    . alterAnnotationsST reactStyle
    . treeForm
    . layoutPretty layoutOptions
  where

    reactStyle :: StgiAnn -> [HtmlClass]
    reactStyle = \case
        StateAnn Headline       -> [HtmlClass "headline"]
        StateAnn Address        -> [HtmlClass "address"]
        StateAnn AddressCore    -> [HtmlClass "addresscore"]
        StateAnn ClosureType    -> [HtmlClass "closuretype"]
        StateAnn StackFrameType -> [HtmlClass "stackframetype"]

        AstAnn Keyword     -> [HtmlClass "keyword"]
        AstAnn Prim        -> [HtmlClass "prim"]
        AstAnn Variable    -> [HtmlClass "variable"]
        AstAnn Constructor -> [HtmlClass "constructor"]
        AstAnn Semicolon   -> [HtmlClass "semicolon"]

    toRawHtmlBuilder :: SimpleDocTree HtmlClass -> LTB.Builder
    toRawHtmlBuilder = \case
        STEmpty -> mempty
        STChar c -> LTB.singleton c
        STText _ t -> LTB.fromText t
        STLine i -> "<br>\n" <> LTB.fromText (T.replicate i " ")
        STAnn (HtmlClass c) x -> "<span class=\"" <> LTB.fromText c <> "\">"
                              <> toRawHtmlBuilder x
                              <> "</span>"
        STConcat xs -> mconcat (map toRawHtmlBuilder xs)

    layoutOptions = defaultLayoutOptions { layoutPageWidth = Unbounded }

newtype HtmlClass = HtmlClass Text
    deriving (Eq, Ord, Show)
