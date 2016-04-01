module Stg.Language where



import           Data.Map (Map)



newtype Program = Program Binds
    deriving (Eq, Ord, Show)

newtype Binds = Binds (Map Var LambdaForm)
    deriving (Eq, Ord, Show)

data LambdaForm = LambdaForm [Var] UpdateFlag [Var] Expr
    deriving (Eq, Ord, Show)

data UpdateFlag = Update | NoUpdate
    deriving (Eq, Ord, Show)

data Rec = NonRecursive | Recursive
    deriving (Eq, Ord, Show)

data Expr = Let Rec Binds Expr
          | Case Expr Alts
          | AppF Var    [Atom]
          | AppC Constr [Atom]
          | AppP PrimOp [Atom]
          | Lit Literal
          deriving (Eq, Ord, Show)

data Alts = AlgebraicAlts AAlts
          | PrimitiveAlts PAlts
          deriving (Eq, Ord, Show)

data AAlts = AAlts [AAlt] DefaultAlt
          deriving (Eq, Ord, Show)

data PAlts = PAlts [PAlt] DefaultAlt
          deriving (Eq, Ord, Show)

data AAlt = AAlt Constr [Var] Expr
    deriving (Eq, Ord, Show)

data PAlt = PAlt Literal Expr
    deriving (Eq, Ord, Show)

data DefaultAlt = DefaultNotBound Expr
                | DefaultBound Atom Expr
    deriving (Eq, Ord, Show)

data Literal = Literal Int
    deriving (Eq, Ord, Show)

data PrimOp = Add | Sub | Mul | Div | Mod
    deriving (Eq, Ord, Show)

data Var = Var String
    deriving (Eq, Ord, Show)

data Atom = AtomVar Var
          | AtomLit Literal
          deriving (Eq, Ord, Show)

data Constr = ConstrPlaceholder
    deriving (Eq, Ord, Show)
