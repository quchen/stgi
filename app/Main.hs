{-# LANGUAGE QuasiQuotes #-}

module Main (main) where



import qualified Data.Text.IO             as T
import           Stg.Language.Prettyprint
import           Stg.Parser



main :: IO ()
main = do
    let prog = [stg|
        test1 = () \n () -> Baz ();
        test2 = () \n () -> let b = () \n () -> Qux () in Quux ();
        test3 = () \n () -> let b = () \n () -> Qux () in let b = () \n () -> Qux () in Quux ();
        test4 = () \n () -> let a = () \n () -> Qux1 ()
                              in let b = () \n () -> Qux2 ()
                                 in let c = () \n () -> Qux3 ()
                                    in let d = () \n () -> Qux4 ()
                                       in let e = () \n () -> Qux5 ()
                                          in let f = () \n () -> Qux6 ()
                                             in Quux ();
        testA = () \u () -> let bar = () \u () -> case +# 2# bar of
                          1# -> let a1 = () \n () -> Unit ();
                                    a2 = (a) \u (b,c) -> case Maybe () of
                                        Just () -> Unit ();
                                        v -> Unot ();
                                    a3 = () \n (a,s,d) -> f (1#)
                                in Hans ();
                          default -> Bar ()
                  in b ();
        test_int_even = () \n (x) -> case x () of
            0# -> True ();
            default -> let x' = (x) \u () -> -# x 1#
                       in odd (x');
        test_int_odd = () \n (x) -> case x () of
            0# -> False ();
            default -> let x' = (x) \u () -> -# 2# 1#
                       in even (x');
        casecase = () \u () ->
            case
                case
                    case
                        case
                            case
                                case
                                    case
                                        Unit ()
                                        of v -> Unit ()
                                    of v -> Unit ()
                                of v -> Unit ()
                            of v -> Unit ()
                        of v -> Unit ()
                    of v -> Unit ()
                of v -> Unit ()
        |]
    T.putStrLn (prettyParserInverseAnsi prog)
