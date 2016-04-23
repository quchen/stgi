{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Parser (tests) where


-- FIXME  Uh oh, this happened in the ParserInverse for algebraic alternatives test:
--     *** Failed!  (after 980 tests and 15 shrinks):
--     AlgebraicAlts [AlgebraicAlt (Constr "D") [] (Let Recursive (Binds [(Var "s",LambdaForm [Var "sYQ"] Update [Var "aS"] (AppF (Var "o") [AtomLit (Literal (-36))])),(Var "y",LambdaForm [] Update [] (AppF (Var "let") []))]) (AppP Add (AtomLit (Literal 0)) (AtomVar (Var "w"))))] (DefaultBound (Var "t") (AppF (Var "j") []))
--     Input AST:
--     D () -> letrec s = (sYQ) \u (aS) -> o (-36#);
--                    y = () \u () -> let ()              -- WAT.
--             in +# 0# w;
--     t -> j ()
--     Parse error:
--     (string):2:36:
--     unexpected '('
--     expecting in or list of free variables
--
--
-- FIXME  Uh oh, this happened in the ParserInverse for primitive alternatives test:
--     *** Failed!  (after 573 tests and 11 shrinks):
--     PrimitiveAlts [PrimitiveAlt (Literal 0) (Case (AppF (Var "i") []) (Alts (AlgebraicAlts [] (DefaultBound (Var "d") (Let NonRecursive (Binds [(Var "hD7G",LambdaForm [] Update [] (AppF (Var "let") []))]) (AppP Mul (AtomLit (Literal 0)) (AtomLit (Literal 0))))))))] (DefaultBound (Var "f") (AppP Mul (AtomLit (Literal 0)) (AtomVar (Var "s"))))
--     Input AST:
--     0# -> case i () of
--               d -> let hD7G = () \u () -> let ()              -- WAT.
--                    in *# 0# 0#;
--     f -> *# 0# s
--     Parse error:
--     (string):2:43:
--     unexpected '('
--     expecting in or list of free variables

import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Parser

import           Test.Orphans             ()



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
    [ shouldParseToSuccess "Simple binding with literal"
        "one = () \\u () -> 1#"
        (Binds [("one", LambdaForm [] Update []
                          (Lit 1) )])

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
        "add1 = () \\n (n) -> +# n 1#"
        (Binds [("add1", LambdaForm [] NoUpdate ["n"]
                             (AppP Add (AtomVar "n") (AtomLit 1)) )])

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
        "fac = () \\n (n) ->                                                 \n\
        \   case n () of                                                     \n\
        \       0#      -> 1#;                                               \n\
        \       default -> let n' = () \\u () -> -# n 1#                     \n\
        \                  in fac (n')                                         "
        (Binds
            [ ("fac", LambdaForm [] NoUpdate ["n"]
                  (Case (AppF "n" []) (Alts
                      [ PrimitiveAlt 0 (Lit 1) ]
                      (DefaultNotBound
                          (Let NonRecursive
                              (Binds
                                  [ ("n'", LambdaForm [] Update []
                                               (AppP Sub (AtomVar "n") (AtomLit 1)) )])
                              (AppF "fac" [AtomVar "n'"]) )))))])

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
