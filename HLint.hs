import "hint" HLint.Default
import "hint" HLint.Builtin.All

-- Naming can be useful
ignore "Avoid lambda"
ignore "Redundant lambda"
ignore "Eta reduce"
ignore "Use camelCase"
ignore "Use fromMaybe"
ignore "Use if"

-- AMP fallout
error "generalize mapM"  = mapM  ==> traverse
error "generalize mapM_" = mapM_ ==> traverse_
error "generalize forM"  = forM  ==> for
error "generalize forM_" = forM_ ==> for_
error "Avoid return" =
    return ==> pure
    where note = "return is obsolete as of GHC 7.10"

error "Use parentheses instead of ($)" = f $ x ==> ()
