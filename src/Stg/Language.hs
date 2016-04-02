module Stg.Language where



import           Data.Map  (Map)
import           Data.Text (Text)



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
      AlgebraicAlts AAlts -- ^ Algebraic alternatives, like in True | False
    | PrimitiveAlts PAlts -- ^ Primitive alternatives, like 1, 2, 3
    deriving (Eq, Ord, Show)

-- | Algebraic alternatives, with a default as fallback.
data AAlts = AAlts [AAlt] DefaultAlt
    deriving (Eq, Ord, Show)

-- | Primitive version of 'AAlts'.
data PAlts = PAlts [PAlt] DefaultAlt
    deriving (Eq, Ord, Show)

-- | A single algebraic alternative, consisting of a constructor with its
-- pattern arguments, and an expression to continue at, should the pattern
-- match.
data AAlt = AAlt Constr [Var] Expr
    deriving (Eq, Ord, Show)

-- | Primitive version of 'AAlt'.
data PAlt = PAlt Literal Expr
    deriving (Eq, Ord, Show)

-- | If no viable alternative is found in a pattern match, use a 'DefaultAlt'
-- as fallback.
data DefaultAlt =
       DefaultNotBound Expr
     | DefaultBound Var Expr
    deriving (Eq, Ord, Show)

-- | Literals are the basis of primitive operations.
data Literal = Literal Int
    deriving (Eq, Ord, Show)

-- | Primitive operations on 'Literal's.
data PrimOp = Add | Sub | Mul | Div | Mod
    deriving (Eq, Ord, Show)

-- | Variable.
data Var = Var Text
    deriving (Eq, Ord, Show)

-- | Smallest unit of data.
data Atom =
      AtomVar Var
    | AtomLit Literal
    deriving (Eq, Ord, Show)

-- | Constructors of algebraic data types.
data Constr = ConstrPlaceholder
    deriving (Eq, Ord, Show)
