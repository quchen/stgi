import "hlint" HLint.Default

-- Naming can be useful
ignore "Avoid Lambda"
ignore "Eta reduce"

-- AMP fallout
error "generalize mapM"  = mapM  ==> traverse
error "generalize mapM_" = mapM_ ==> traverse_
error "generalize forM"  = forM  ==> for
error "generalize forM_" = forM_ ==> for_
error "Avoid return" =
    return ==> pure
    where note = "return is obsolete as of GHC 7.10"

error "Use parentheses instead of ($)" = f $ x ==> ()
