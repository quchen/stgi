{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The STG language syntax tree, modeled after the description in the
-- 1992 paper
-- <http://research.microsoft.com/apps/pubs/default.aspx?id=67083 (link)>.
--
-- A 'Program' is typically created using functionality provided by the
-- "Stg.Parser" module, as opposed to manually combining the data types given
-- in this module.
--
-- For example, the STG program
--
-- @
-- fix = () \\n (f) ->
--     letrec x = (f, x) \\u () -> f (x)
--     in x ()
-- @
--
-- is represented by
--
-- @
-- 'Binds'
--     [("fix", 'LambdaForm' [] 'NoUpdate' ["f"]
--         ('Let' 'Recursive'
--             ('Binds' [("x", 'LambdaForm' ["f","x"] 'Update' []
--                 ('AppF' "f" ['AtomVar' "x"]))])
--             ('AppF' "x" [])))]
-- @
module Stg.Language (
    Program      (..),
    Binds        (..),
    LambdaForm   (..),
    UpdateFlag   (..),
    Rec          (..),
    Expr         (..),
    Alts         (..),
    AlgebraicAlts(..),
    PrimitiveAlts(..),
    AlgebraicAlt (..),
    PrimitiveAlt (..),
    DefaultAlt   (..),
    Literal      (..),
    PrimOp       (..),
    Var          (..),
    Atom         (..),
    Constr       (..),
) where



import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Exts
import           GHC.Generics
import           Language.Haskell.TH.Lift



-- | An STG program consists of bindings.
newtype Program = Program Binds
    deriving (Eq, Ord, Show, Generic)

-- | Bindings are collections of lambda forms, indexed over variables.
newtype Binds = Binds (Map Var LambdaForm)
    deriving (Eq, Ord, Generic)

instance Show Binds where
    show (Binds binds) = "(Binds " <> show (M.toList binds) <> ")"

-- | A lambda form unifies free and bound variables associated with a function
-- body.
data LambdaForm = LambdaForm [Var] UpdateFlag [Var] Expr
    deriving (Eq, Ord, Show, Generic)

-- | The update flag distinguishes updateable from non-updateable lambda forms.
--
-- The former will be overwritten in-place when it is evaluated, allowing
-- the calculation of a thunk to be shared among multiple uses of the
-- same value.
data UpdateFlag = Update | NoUpdate
    deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Distinguishes @let@ from @letrec@.
data Rec = NonRecursive | Recursive
    deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- | An expression in the STG language.
data Expr =
      Let Rec Binds Expr    -- ^ Let expression @let(rec) ... in ...@
    | Case Expr Alts        -- ^ Case expression @case ... of ... x -> y@
    | AppF Var [Atom]       -- ^ Function application @f (x,y,z)@
    | AppC Constr [Atom]    -- ^ Constructor application @Maybe (a)@
    | AppP PrimOp Atom Atom -- ^ Primitive function application @+# 1# 2#@
    | Lit Literal           -- ^ Literal expression @1#@
    deriving (Eq, Ord, Show, Generic)

-- | List of possible alternatives in a 'Case' expression.
data Alts =
      Algebraic AlgebraicAlts -- ^ as in @True | False@
    | Primitive PrimitiveAlts -- ^ like 1, 2, 3
    deriving (Eq, Ord, Show, Generic)

-- | Algebraic alternatives, with a default as fallback.
data AlgebraicAlts = AlgebraicAlts [AlgebraicAlt] DefaultAlt
    deriving (Eq, Ord, Show, Generic)

-- | Primitive version of 'AlgebraicAlts'.
data PrimitiveAlts = PrimitiveAlts [PrimitiveAlt] DefaultAlt
    deriving (Eq, Ord, Show, Generic)

-- | A single algebraic alternative, consisting of a constructor with its
-- pattern arguments, and an expression to continue at, should the pattern
-- match.
data AlgebraicAlt = AlgebraicAlt Constr [Var] Expr
    deriving (Eq, Ord, Show, Generic)

-- | Primitive version of 'AlgebraicAlt'.
data PrimitiveAlt = PrimitiveAlt Literal Expr
    deriving (Eq, Ord, Show, Generic)

-- | If no viable alternative is found in a pattern match, use a 'DefaultAlt'
-- as fallback.
data DefaultAlt =
       DefaultNotBound Expr
     | DefaultBound Var Expr
    deriving (Eq, Ord, Show, Generic)

-- | Literals are the basis of primitive operations.
newtype Literal = Literal Int
    deriving (Eq, Ord, Show, Generic)

instance Num Literal where
    Literal x + Literal y = Literal (x + y)
    Literal x * Literal y = Literal (x * y)
    Literal x - Literal y = Literal (x - y)

    abs    (Literal i) = Literal (abs i)
    signum (Literal i) = Literal (abs i)
    negate (Literal i) = Literal (negate i)

    fromInteger = Literal . fromInteger

-- | Primitive operations.
data PrimOp = Add | Sub | Mul | Div | Mod
    deriving (Eq, Ord, Show, Generic, Bounded, Enum)

-- | Variable.
newtype Var = Var Text
    deriving (Eq, Ord, Show, Generic)

instance IsString Var where fromString = coerce . T.pack

-- | Smallest unit of data.
data Atom =
      AtomVar Var
    | AtomLit Literal
    deriving (Eq, Ord, Show, Generic)

-- | Constructors of algebraic data types.
newtype Constr = Constr Text
    deriving (Eq, Ord, Show, Generic)

instance IsString Constr where fromString = coerce . T.pack



--------------------------------------------------------------------------------
-- Lift instances
deriveLiftMany [ ''Program, ''Literal, ''LambdaForm , ''UpdateFlag, ''Rec
               , ''Expr, ''Alts , ''AlgebraicAlts , ''PrimitiveAlts
               , ''AlgebraicAlt , ''PrimitiveAlt, ''DefaultAlt , ''PrimOp
               , ''Atom ]

instance Lift Binds where
    lift (Binds binds) = [| Binds (M.fromList $(lift (M.toList binds))) |]

instance Lift Constr where
    lift (Constr con) = [| Constr (T.pack $(lift (T.unpack con))) |]

instance Lift Var where
    lift (Var var) = [| Var (T.pack $(lift (T.unpack var))) |]
