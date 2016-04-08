{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Parser (tests) where



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
tests = testGroup "Explicit parses" [simpleParses]

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
                                (Algebraic (AlgebraicAlts [] (DefaultBound "y" (AppF "y" [])))) ))])

    , shouldParseToSuccess "Primitive function application"
        "add1 = () \\n (n) -> +# n 1#"
        (Binds [("add1", LambdaForm [] NoUpdate ["n"]
                             (AppP Add (AtomVar "n") (AtomLit 1)) )])

    , shouldParseToSuccess "Let"
        "a = () \\n () ->\n\
        \    let y = (a) \\u (x) -> Foo (x)\n\
        \    in Con (y)"
       (Binds [("a", LambdaForm [] NoUpdate []
                         (Let NonRecursive (Binds
                             [("y", LambdaForm ["a"] Update ["x"]
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
                  (Case (AppF "n" []) (Primitive
                      (PrimitiveAlts
                          [ PrimitiveAlt 0 (Lit 1) ]
                          (DefaultNotBound
                              (Let NonRecursive
                                  (Binds
                                      [ ("n'", LambdaForm [] Update []
                                                   (AppP Sub (AtomVar "n") (AtomLit 1)) )])
                                  (AppF "fac" [AtomVar "n'"]) ))))))])

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
                 (Case (AppF "xs" []) (Algebraic
                     (AlgebraicAlts
                         [ AlgebraicAlt "Nil" []
                               (AppC "Nil" [])
                         , AlgebraicAlt "Cons" ["y","ys"]
                               (Let NonRecursive
                                   (Binds [ ("fy", LambdaForm ["f","y"] Update []
                                                       (AppF "f" [AtomVar "y"]))
                                          , ("mfy", LambdaForm ["f","ys"] Update []
                                                        (AppF "map" [AtomVar "f", AtomVar "ys"])) ])
                                   (AppC "Cons" [AtomVar "fy", AtomVar "mfy"])) ]
                         (DefaultNotBound (AppF "badListError" [])) ))))])

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
                                (Case (AppF "xs" []) (Algebraic
                                    (AlgebraicAlts
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
                                    (DefaultNotBound (AppF "badListError" [])) ))))])
                      (AppF "mf" [])))])
    ]

shouldParseToSuccess :: Text -> Text -> Binds -> TestTree
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
       Right r  -> prettyprint r
    test = assertEqual (T.unpack failMessage) expected actual
