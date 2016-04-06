-- | The STG language syntax tree, modeled after the decription in the 1992
-- paper.
module Stg.Language where



import           Data.Map    (Map)
import           Data.Monoid
import           Data.Text   (Text, pack, unpack)
import           GHC.Exts



newtype Program = Program Binds
    deriving (Eq, Ord, Show)

newtype Binds = Binds (Map Var LambdaForm)
    deriving (Eq, Ord, Show)

data LambdaForm = LambdaForm [Var] UpdateFlag [Var] Expr
    deriving (Eq, Ord, Show)

data UpdateFlag = Update | NoUpdate
    deriving (Eq, Ord, Show)

-- | Distinguish @let@ from @letrec@
data Rec = NonRecursive | Recursive
    deriving (Eq, Ord, Show)

-- | An expression in the STG language.
data Expr =
      Let Rec Binds Expr    -- ^ let(rec) ... in ...
    | Case Expr Alts        -- ^ case ... of ... x -> y
    | AppF Var [Atom]       -- ^ Function application
    | AppC Constr [Atom]    -- ^ Constructor application
    | AppP PrimOp Atom Atom -- ^ Primitive function application
    | Lit Literal           -- ^ Literal expression
    deriving (Eq, Ord, Show)

-- | List of possible alternatives in a 'Case' expression.
data Alts =
      Algebraic AlgebraicAlts -- ^ as in True | False
    | Primitive PrimitiveAlts -- ^ like 1, 2, 3
    deriving (Eq, Ord, Show)

-- | Algebraic alternatives, with a default as fallback.
data AlgebraicAlts = AlgebraicAlts [AlgebraicAlt] DefaultAlt
    deriving (Eq, Ord, Show)

-- | Primitive version of 'AlgebraicAlts'.
data PrimitiveAlts = PrimitiveAlts [PrimitiveAlt] DefaultAlt
    deriving (Eq, Ord, Show)

-- | A single algebraic alternative, consisting of a constructor with its
-- pattern arguments, and an expression to continue at, should the pattern
-- match.
data AlgebraicAlt = AlgebraicAlt Constr [Var] Expr
    deriving (Eq, Ord, Show)

-- | Primitive version of 'AlgebraicAlt'.
data PrimitiveAlt = PrimitiveAlt Literal Expr
    deriving (Eq, Ord, Show)

-- | If no viable alternative is found in a pattern match, use a 'DefaultAlt'
-- as fallback.
data DefaultAlt =
       DefaultNotBound Expr
     | DefaultBound Var Expr
    deriving (Eq, Ord, Show)

-- | Literals are the basis of primitive operations.
newtype Literal = Literal Int
    deriving (Eq, Ord)

instance Show Literal where show (Literal i) = show i

instance Num Literal where
    Literal x + Literal y = Literal (x + y)
    Literal x * Literal y = Literal (x * y)
    Literal x - Literal y = Literal (x - y)

    abs    (Literal i) = Literal (abs i)
    signum (Literal i) = Literal (abs i)
    negate (Literal i) = Literal (negate i)

    fromInteger = Literal . fromInteger

-- | Primitive operations on 'Literal's.
data PrimOp = Add | Sub | Mul | Div | Mod
    deriving (Eq, Ord, Show)

-- | Variable.
newtype Var = Var Text
    deriving (Eq, Ord)

instance IsString Var where fromString = coerce . pack
instance Show Var where show (Var v) = "\"" <> unpack v <> "\""

-- | Smallest unit of data.
data Atom =
      AtomVar Var
    | AtomLit Literal
    deriving (Eq, Ord, Show)

-- | Constructors of algebraic data types.
newtype Constr = Constr Text
    deriving (Eq, Ord)

instance IsString Constr where fromString = coerce . pack
instance Show Constr where show (Constr c) = "\"" <> unpack c <> "\""
