{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Parser (tests) where

import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Test.Tasty
import           Test.Tasty.HUnit

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Parser

import Test.Orphans ()



tests :: TestTree
tests = testGroup "Explicit parses"
    [ simpleParses
    , badParses ]



shouldParseToSuccess
    :: Text  -- ^ Test name
    -> Text  -- ^ Parser input
    -> Binds -- ^ Expected STG bindings
    -> TestTree
shouldParseToSuccess testName input output = testCase (T.unpack testName) test
  where
    actual = parse input
    expected = Right (Program output)
    failMessage = case actual of
       Left err -> T.unlines
          [ "============="
          , "Could not parse"
          , (T.unlines . map (" > " <>) . T.lines) input
          , "Error encountered:"
          , (T.unlines . map (" > " <>) . T.lines) err
          , "=============" ]
       Right r -> prettyprintParserInverse r
    test = assertEqual (T.unpack failMessage) expected actual



simpleParses :: TestTree
simpleParses = testGroup "Well-written programs"
    [ shouldParseToSuccess "Simple binding to boxed literal"
        "one = () \\u () -> Int# (1#)"
        (Binds [("one", LambdaForm [] Update []
                          (AppC "Int#" [AtomLit 1]) )])

    , shouldParseToSuccess "Constructor application"
        "con = () \\n () -> Maybe (b, 1#)"
        (Binds [("con", LambdaForm [] NoUpdate []
                          (AppC "Maybe"
                                 [AtomVar "b" , AtomLit 1] ))])

    , shouldParseToSuccess "Bound pattern"
        "id = () \\n (x) -> case x () of y -> y ()"
        (Binds [("id", LambdaForm [] NoUpdate ["x"]
                          (Case (AppF "x" [])
                                (Alts [] (DefaultBound "y" (AppF "y" []))) ))])

    , shouldParseToSuccess "Primitive function application"
        "add1 = () \\n (n) -> case +# n 1# of n' -> Int# (n')"
        (Binds [("add1", LambdaForm [] NoUpdate ["n"]
                            (Case (AppP Add (AtomVar "n") (AtomLit 1))
                                (Alts [] (DefaultBound "n'" (AppC "Int#" [AtomVar "n'"])))))])


    , shouldParseToSuccess "Let"
        "a = () \\n () ->\n\
        \    let y = (a) \\n (x) -> Foo (x)\n\
        \    in Con (y)"
       (Binds [("a", LambdaForm [] NoUpdate []
                         (Let NonRecursive (Binds
                             [("y", LambdaForm ["a"] NoUpdate ["x"]
                                        (AppC "Foo" [AtomVar "x"]))])
                             (AppC "Con" [AtomVar "y"])))])

    , shouldParseToSuccess "fix"
        "fix = () \\n (f) -> \n\
        \    letrec x = (f, x) \\u () -> f (x) in x ()"
        (Binds
            [("fix", LambdaForm [] NoUpdate ["f"]
                         (Let Recursive
                             (Binds [("x", LambdaForm ["f","x"] Update []
                                         (AppF "f" [AtomVar "x"]))])
                             (AppF "x" [])))])

    , shouldParseToSuccess "factorial"
        "fac = () \\n (n) ->                                                  \n\
        \   case n () of                                                      \n\
        \       0#      -> Int# (1#);                                         \n\
        \       default -> case -# n 1# of                                    \n\
        \           nMinusOne ->                                              \n\
        \                let fac' = (nMinusOne) \\u () -> fac (nMinusOne)     \n\
        \                in case fac' () of                                   \n\
        \                    Int# (facNMinusOne) -> case *# n facNMinusOne of \n\
        \                        result -> Int# (result);                     \n\
        \                    err -> Error_fac (err)                           "
        (Binds
            [(Var "fac",LambdaForm [] NoUpdate [Var "n"]
                (Case (AppF (Var "n") []) (Alts
                    [PrimitiveAlt (Literal 0)
                                  (AppC (Constr "Int#")
                                        [AtomLit (Literal 1)] )]
                    (DefaultNotBound
                        (Case (AppP Sub (AtomVar (Var "n")) (AtomLit (Literal 1))) (Alts
                            []
                            (DefaultBound (Var "nMinusOne")
                                (Let NonRecursive
                                    (Binds
                                        [(Var "fac'",LambdaForm [Var "nMinusOne"] Update []
                                            (AppF (Var "fac") [AtomVar (Var "nMinusOne")]) )])
                                    (Case (AppF (Var "fac'") []) (Alts
                                        [AlgebraicAlt (Constr "Int#") [Var "facNMinusOne"]
                                            (Case (AppP Mul (AtomVar (Var "n")) (AtomVar (Var "facNMinusOne"))) (Alts
                                                []
                                                (DefaultBound (Var "result") (AppC (Constr "Int#") [AtomVar (Var "result")])) ))]
                                        (DefaultBound (Var "err") (AppC (Constr "Error_fac") [AtomVar (Var "err")])) ))))))))))])

   , shouldParseToSuccess "map with comment"
        "-- Taken from the 1992 STG paper, page 21.                          \n\
        \map = () \\n (f, xs) ->                                             \n\
        \    case xs () of                                                   \n\
        \        Nil () -> Nil ();                                           \n\
        \        Cons (y,ys) -> let fy = (f,y) \\u () -> f (y);              \n\
        \                           mfy = (f,ys) \\u () -> map (f,ys)        \n\
        \                       in Cons (fy,mfy);                            \n\
        \        default -> badListError ()                                  "
       (Binds
           [ ("map", LambdaForm [] NoUpdate ["f","xs"]
                 (Case (AppF "xs" []) (Alts
                     [ AlgebraicAlt "Nil" []
                           (AppC "Nil" [])
                     , AlgebraicAlt "Cons" ["y","ys"]
                           (Let NonRecursive
                               (Binds [ ("fy", LambdaForm ["f","y"] Update []
                                                   (AppF "f" [AtomVar "y"]))
                                      , ("mfy", LambdaForm ["f","ys"] Update []
                                                    (AppF "map" [AtomVar "f", AtomVar "ys"])) ])
                               (AppC "Cons" [AtomVar "fy", AtomVar "mfy"])) ]
                     (DefaultNotBound (AppF "badListError" [])) )))])

    , shouldParseToSuccess "map, differently implemented"
         "-- Taken from the 1992 STG paper, page 22.                         \n\
         \map = () \\n (f) ->                                                \n\
         \    letrec mf = (f,mf) \\n (xs) ->                                 \n\
         \        case xs () of                                              \n\
         \            Nil () -> Nil ();                                      \n\
         \            Cons (y,ys) -> let fy = (f,y) \\u () -> f (y);         \n\
         \                               mfy = (mf, ys) \\u () -> mf (ys)    \n\
         \                           in Cons (fy, mfy);                      \n\
         \            default -> badListError ()                             \n\
         \    in mf ()                                                       "
        (Binds
            [ ("map", LambdaForm [] NoUpdate ["f"]
                  (Let Recursive
                      (Binds
                          [ ("mf", LambdaForm ["f","mf"] NoUpdate ["xs"]
                                (Case (AppF "xs" []) (Alts
                                        [ AlgebraicAlt "Nil" []
                                              (AppC "Nil" [])
                                        , AlgebraicAlt "Cons" ["y","ys"]
                                              (Let NonRecursive
                                                  (Binds
                                                      [ ("fy", LambdaForm ["f","y"] Update []
                                                            (AppF "f" [AtomVar "y"]))
                                                      , ("mfy", LambdaForm ["mf","ys"] Update []
                                                            (AppF "mf" [AtomVar "ys"]) )])
                                                  (AppC "Cons" [AtomVar "fy", AtomVar "mfy"]) )]
                                    (DefaultNotBound (AppF "badListError" [])) )))])
                      (AppF "mf" [])))])
    ]

shouldFailToParse
    :: Text -- ^ Test name
    -> Text -- ^ Parser input
    -> TestTree
shouldFailToParse testName input = testCase (T.unpack testName) test
  where
    test = case parse input of
        Right ast -> (assertFailure . T.unpack . T.unlines)
            [ "Parser should have failed, but succeeded to parse to"
            , (T.unlines . map (" > " <>) . T.lines . prettyprintAnsi) ast ]
        Left _err -> pure ()

badParses :: TestTree
badParses = testGroup "Parsers that should fail"
    [ shouldFailToParse "Updatable lambda forms don't take arguments"
        "x = () \\u (y) -> z ()"
    ]
