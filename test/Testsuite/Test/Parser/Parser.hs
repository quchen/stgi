{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Parser.Parser (tests) where



import           Data.Bifunctor
import           Data.Monoid
import           Data.Text      (Text)
import qualified Data.Text      as T

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Parser.Parser
import qualified Stg.Parser.QuasiQuoter   as QQ

import Test.Orphans     ()
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Hand-written cases"
    [ simpleParses
    , badParses
    , stresstest ]



shouldParseTo
    :: Text  -- ^ Test name
    -> Text  -- ^ Parser input
    -> Binds -- ^ Expected STG bindings
    -> TestTree
shouldParseTo testName input output = testCase (T.unpack testName) test
  where
    actual = first prettyprint (parse program input)
    expected = Right (Program output)
    failMessage = case actual of
       Left err -> T.unlines
          [ "============="
          , "Could not parse"
          , (T.unlines . map (" > " <>) . T.lines) input
          , "Error encountered:"
          , (T.unlines . map (" > " <>) . T.lines) err
          , "=============" ]
       Right r -> prettyprint r
    test = assertEqual (T.unpack failMessage) expected actual



simpleParses :: TestTree
simpleParses = testGroup "Well-written programs"
    [ shouldParseTo "Simple binding to boxed literal"
        "one = \\ -> Int# 1#"
        (Binds [("one", LambdaForm [] NoUpdate []
                          (AppC "Int#" [AtomLit 1]) )])

    , shouldParseTo "Constructor application"
        "con = \\ -> Maybe b 1#"
        (Binds [("con", LambdaForm [] NoUpdate []
                          (AppC "Maybe"
                                 [AtomVar "b" , AtomLit 1] ))])

    , shouldParseTo "Bound pattern"
        "id = \\ x -> case x of y -> y"
        (Binds [("id", LambdaForm [] NoUpdate ["x"]
                          (Case (AppF "x" [])
                                (Alts NoNonDefaultAlts
                                      (DefaultBound "y" (AppF "y" []))) ))])

    , shouldParseTo "Primitive function application"
        "add1 = \\n -> case +# n 1# of n' -> Int# n'"
        (Binds [("add1", LambdaForm [] NoUpdate ["n"]
                            (Case (AppP Add (AtomVar "n") (AtomLit 1))
                                (Alts NoNonDefaultAlts
                                      (DefaultBound "n'" (AppC "Int#" [AtomVar "n'"])))))])


    , shouldParseTo "Let"
        "a = \\ ->                                                           \n\
        \    let y = \\(a) x -> Foo x                                        \n\
        \    in Con y"
       (Binds [("a", LambdaForm [] NoUpdate []
                         (Let NonRecursive (Binds
                             [("y", LambdaForm ["a"] NoUpdate ["x"]
                                        (AppC "Foo" [AtomVar "x"]))])
                             (AppC "Con" [AtomVar "y"])))])

    , shouldParseTo "fix"
        "fix = \\f ->                                                        \n\
        \    letrec x = \\(f x) => f x in x"
        (Binds
            [("fix", LambdaForm [] NoUpdate ["f"]
                         (Let Recursive
                             (Binds [("x", LambdaForm ["f","x"] Update []
                                         (AppF "f" [AtomVar "x"]))])
                             (AppF "x" [])))])

    , shouldParseTo "factorial"
        "fac = \\n ->                                                        \n\
        \   case n of                                                        \n\
        \       0#      -> Int# 1#;                                          \n\
        \       default -> case -# n 1# of                                   \n\
        \           nMinusOne ->                                             \n\
        \                let fac' = \\(nMinusOne) => fac nMinusOne           \n\
        \                in case fac' of                                     \n\
        \                    Int# facNMinusOne -> case *# n facNMinusOne of  \n\
        \                        result -> Int# result;                      \n\
        \                    err -> Error_fac err                            "
        (Binds
            [(Var "fac",LambdaForm [] NoUpdate [Var "n"]
                (Case (AppF (Var "n") []) (Alts (PrimitiveAlts
                    [PrimitiveAlt (Literal 0)
                                  (AppC (Constr "Int#")
                                        [AtomLit (Literal 1)] )])
                    (DefaultNotBound
                        (Case (AppP Sub (AtomVar (Var "n")) (AtomLit (Literal 1))) (Alts
                            NoNonDefaultAlts
                            (DefaultBound (Var "nMinusOne")
                                (Let NonRecursive
                                    (Binds
                                        [(Var "fac'",LambdaForm [Var "nMinusOne"] Update []
                                            (AppF (Var "fac") [AtomVar (Var "nMinusOne")]) )])
                                    (Case (AppF (Var "fac'") []) (Alts (AlgebraicAlts
                                        [AlgebraicAlt (Constr "Int#") [Var "facNMinusOne"]
                                            (Case (AppP Mul (AtomVar (Var "n")) (AtomVar (Var "facNMinusOne"))) (Alts
                                                NoNonDefaultAlts
                                                (DefaultBound (Var "result") (AppC (Constr "Int#") [AtomVar (Var "result")])) ))])
                                        (DefaultBound (Var "err") (AppC (Constr "Error_fac") [AtomVar (Var "err")])) ))))))))))])

   , shouldParseTo "map with comment"
        "-- Taken from the 1992 STG paper, page 21.                          \n\
        \map = \\f xs ->                                                     \n\
        \    case xs of                                                      \n\
        \        Nil -> Nil;                                                 \n\
        \        Cons y ys -> let fy = \\(f y) => f y;                       \n\
        \                           mfy = \\(f ys) => map f ys               \n\
        \                       in Cons fy mfy;                              \n\
        \        default -> badListError                                     "
       (Binds
           [ ("map", LambdaForm [] NoUpdate ["f","xs"]
                 (Case (AppF "xs" []) (Alts (AlgebraicAlts
                     [ AlgebraicAlt "Nil" []
                           (AppC "Nil" [])
                     , AlgebraicAlt "Cons" ["y","ys"]
                           (Let NonRecursive
                               (Binds [ ("fy", LambdaForm ["f","y"] Update []
                                                   (AppF "f" [AtomVar "y"]))
                                      , ("mfy", LambdaForm ["f","ys"] Update []
                                                    (AppF "map" [AtomVar "f", AtomVar "ys"])) ])
                               (AppC "Cons" [AtomVar "fy", AtomVar "mfy"])) ])
                     (DefaultNotBound (AppF "badListError" [])) )))])

    , shouldParseTo "map, differently implemented"
         "-- Taken from the 1992 STG paper, page 22.                         \n\
         \map = \\f ->                                                       \n\
         \    letrec mf = \\(f mf) xs ->                                     \n\
         \        case xs of                                                 \n\
         \            Nil -> Nil;                                            \n\
         \            Cons y ys -> let fy = \\(f y) => f y;                  \n\
         \                             mfy = \\(mf ys) => mf ys              \n\
         \                           in Cons fy mfy;                         \n\
         \            default -> badListError                                \n\
         \    in mf                                                          "
        (Binds
            [ ("map", LambdaForm [] NoUpdate ["f"]
                  (Let Recursive
                      (Binds
                          [ ("mf", LambdaForm ["f","mf"] NoUpdate ["xs"]
                                (Case (AppF "xs" []) (Alts (AlgebraicAlts
                                        [ AlgebraicAlt "Nil" []
                                              (AppC "Nil" [])
                                        , AlgebraicAlt "Cons" ["y","ys"]
                                              (Let NonRecursive
                                                  (Binds
                                                      [ ("fy", LambdaForm ["f","y"] Update []
                                                            (AppF "f" [AtomVar "y"]))
                                                      , ("mfy", LambdaForm ["mf","ys"] Update []
                                                            (AppF "mf" [AtomVar "ys"]) )])
                                                  (AppC "Cons" [AtomVar "fy", AtomVar "mfy"]) )])
                                    (DefaultNotBound (AppF "badListError" [])) )))])
                      (AppF "mf" [])))])
    ]

shouldFailToParse
    :: Text -- ^ Test name
    -> Text -- ^ Parser input
    -> TestTree
shouldFailToParse testName input = testCase (T.unpack testName) test
  where
    test = case parse program input of
        Right ast -> (assertFailure . T.unpack . T.unlines)
            [ "Parser should have failed, but succeeded to parse to"
            , (T.unlines . map (" > " <>) . T.lines . prettyprint) ast ]
        Left _err -> pure ()

badParses :: TestTree
badParses = testGroup "Parsers that should fail"
    [ shouldFailToParse "Updatable lambda forms don't take arguments"
        "x = \\y => z"
    , shouldFailToParse "Standard constructors are not updatable"
        "x = \\(y) => Con y"
    ]

stresstest :: TestTree
stresstest = testGroup "Stress test"
    [ shouldParseTo "As few as possible spaces"
        "x=\\y->case x of default->z"
        [QQ.binds| x = \y -> case x of default -> z |]
    , testGroup "Too few spaces"
        [ shouldFailToParse "casex of"
            "x=\\y->casex of default->z"
        , shouldFailToParse "case xof"
            "x=\\y->case xof default->z"
        , shouldFailToParse "ofdefault"
            "x=\\y->case x ofdefault->z"
        ]
    ]
