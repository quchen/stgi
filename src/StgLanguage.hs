module StgLanguage where



import           Data.Map (Map)



newtype Program = Program Binds
data Binds = Binds (Map Var LambdaForm)
data LambdaForm = LambdaForm [Var] UpdateFlag [Var] Expr
data UpdateFlag = Update | NoUpdate
data Expr = Let Binds Expr
          | LetRec Binds Expr
          | Case Expr Alts
          | AppF Var [Atom]
          | AppC Constr [Atom]
          | AppP Prim [Atom]
          | Lit Literal
data Alts = AlgebraicAlts [AAlt] DefaultAlt
          | PrimitiveAlts [PAlt] DefaultAlt
data AAlt = AAlt Constr [Var] Expr
data PAlt = PAlt Literal Expr
data DefaultAlt = VarDefault
data Literal = Literal Int
data Prim = Prim (Int -> Int -> Int)
data Var = Var String deriving (Eq, Ord, Show)
data Atom = AtomVar Var
          | AtomLit Literal
data Constr = ConstrPlaceholder
