{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The STG language syntax tree, modeled after the description in the
-- 1992 paper
-- <http://research.microsoft.com/apps/pubs/default.aspx?id=67083 (link)>.
--
-- A 'Program' is typically created using functionality provided by the
-- "Stg.Parser" module, as opposed to manually combining the data types given
-- in this module.
--
-- For plenty of comparisons of STG language source and generated parse trees,
-- have a look at the "Stg.Parser.QuasiQuoter" module.
module Stg.Language (
    Program        (..),
    Binds          (..),
    LambdaForm     (..),
    prettyLambda,
    UpdateFlag     (..),
    Rec            (..),
    Expr           (..),
    Alts           (..),
    NonDefaultAlts (..),
    AlgebraicAlt   (..),
    PrimitiveAlt   (..),
    DefaultAlt     (..),
    Literal        (..),
    PrimOp         (..),
    Var            (..),
    Atom           (..),
    Constr         (..),
    Pretty         (..),

    -- * Meta information
    classify,
    LambdaType(..),
) where



import           Control.DeepSeq
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Monoid                  hiding (Alt)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           GHC.Exts
import           GHC.Generics
import           Language.Haskell.TH.Lift
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Stg.Parser.QuasiQuoter



-- | Package of style definitions used for prettyprinting the STG AST.
data StgAstStyle = StgAstStyle
    { keyword :: Doc -> Doc
        -- ^ Keyword style
    , prim :: Doc -> Doc
        -- ^ Primitive style, for literals and functions
    , variable :: Doc -> Doc
        -- ^ Variable style
    , constructor :: Doc -> Doc
        -- ^ Constructor style
    , semicolon :: Doc -> Doc
        -- ^ Semicolons separating lists of bindings and alternatives
    }

-- | Colour definitions used by the STG AST.
style :: StgAstStyle
style = StgAstStyle
    { keyword     = id
    , prim        = dullgreen
    , variable    = dullyellow
    , constructor = dullmagenta
    , semicolon   = dullwhite
    }



-- | An STG 'Program' is the unit that can be loaded by the STG machine. It
-- consists of a set of bindings.
newtype Program = Program Binds
    deriving (Eq, Ord, Show, Generic)

-- | __Right-biased union__ of the contained bindings. This makes for a poor man's
-- module system by appending multiple, potentially partially incomplete,
-- 'Programs' to each other.
--
-- @
-- 'Stg.Prelude.map' <> 'Stg.Prelude.filter' <> ['Stg.Parser.QuasiQuoter.stg'| … actual source … |]
-- @
instance Monoid Program where
    mempty = Program mempty
    Program x `mappend` Program y = Program (x <> y)

-- | Bindings are collections of lambda forms, indexed over variables.
--
-- They exist at the top level, or as part of a let(rec) binding.
newtype Binds = Binds (Map Var LambdaForm)
    deriving (Eq, Ord, Generic)

-- | __Right-biased__ union. See 'Monoid' 'Program' for further information.
instance Monoid Binds where
    mempty = Binds mempty
    Binds x `mappend` Binds y = Binds (y <> x)

instance Show Binds where
    show (Binds binds) = "(Binds " <> show (M.assocs binds) <> ")"

-- | A lambda form unifies free and bound variables associated with a function
-- body. The lambda body must not be of primitive type, as this would imply
-- the value is both boxed and unboxed.
--
-- >>> [stg| \(x) y z -> expr x z |]
-- LambdaForm [Var "x"] NoUpdate [Var "y",Var "z"] (AppF (Var "expr") [AtomVar (Var "x"),AtomVar (Var "z")])
data LambdaForm = LambdaForm ![Var] !UpdateFlag ![Var] !Expr
    -- ^ * Free variables (excluding globals)
    --   * Update flag
    --   * Bound variables
    --   * Body
    deriving (Eq, Ord, Show, Generic)

-- | Possible classification of lambda forms.
data LambdaType =
      LambdaCon   -- ^ Data constructor ('AppC' as body)
    | LambdaFun   -- ^ Function (lambda with non-empty argument list)
    | LambdaThunk -- ^ Thunk (everything else)
    deriving (Eq, Ord, Show)

instance Pretty LambdaType where
    pretty = \case
        LambdaCon -> "Con"
        LambdaFun -> "Fun"
        LambdaThunk -> "Thunk"

-- | Classify the type of a lambda form based on its shape.
classify :: LambdaForm -> LambdaType
classify = \case
    LambdaForm _ _ [] AppC{} -> LambdaCon
    LambdaForm _ _ (_:_) _   -> LambdaFun
    LambdaForm _ _ []    _   -> LambdaThunk

-- | The update flag distinguishes updatable from non-updatable lambda forms.
data UpdateFlag =
      Update -- ^ Overwrite the heap object in-place with its reduced value
             -- once available, making recurring access cheap
    | NoUpdate -- ^ Don't touch the heap object after evaluation
    deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Distinguishes @let@ from @letrec@.
data Rec =
      NonRecursive -- ^ Binings have no access to each other
    | Recursive -- ^ Bindings can be given to each other as free variables
    deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- | An expression in the STG language.
data Expr =
      Let !Rec !Binds !Expr    -- ^ Let expression @let(rec) ... in ...@
    | Case !Expr !Alts         -- ^ Case expression @case ... of ... x -> y@
    | AppF !Var ![Atom]        -- ^ Function application @f x y z@
    | AppC !Constr ![Atom]     -- ^ Saturated constructor application @Just a@
    | AppP !PrimOp !Atom !Atom -- ^ Primitive function application @+# 1# 2#@
    | Lit !Literal             -- ^ Literal expression @1#@
    deriving (Eq, Ord, Show, Generic)

-- | List of possible alternatives in a 'Case' expression.
--
-- The list of alts has to be homogeneous. This is not ensured by the type
-- system, and should be handled by the parser instead.
data Alts = Alts !NonDefaultAlts !DefaultAlt
    deriving (Eq, Ord, Show, Generic)

-- | The part of a 'Case' alternative that's not the default.
data NonDefaultAlts =
      NoNonDefaultAlts
        -- ^ Used in 'case' statements that consist only of a default
        -- alternative. These can be useful to force or unpack values.

    | AlgebraicAlts !(NonEmpty AlgebraicAlt)
        -- ^ Algebraic alternative, like @Cons x xs@.

    | PrimitiveAlts !(NonEmpty PrimitiveAlt)
        -- ^ Primitive alternative, like @1#@.
    deriving (Eq, Ord, Show, Generic)

-- | As in @True | False@
data AlgebraicAlt = AlgebraicAlt !Constr ![Var] !Expr
    deriving (Eq, Ord, Show, Generic)

-- | As in @1#@, @2#@, @3#@
data PrimitiveAlt = PrimitiveAlt !Literal !Expr
    deriving (Eq, Ord, Show, Generic)

-- | If no viable alternative is found in a pattern match, use a 'DefaultAlt'
-- as fallback.
data DefaultAlt =
       DefaultNotBound !Expr
     | DefaultBound !Var !Expr
    deriving (Eq, Ord, Show, Generic)

-- | Literals are the basis of primitive operations.
newtype Literal = Literal Integer
    deriving (Eq, Ord, Show, Generic)

-- | Primitive operations.
data PrimOp =
      Add -- ^ @+@
    | Sub -- ^ @-@
    | Mul -- ^ @*@
    | Div -- ^ @/@
    | Mod -- ^ @%@
    | Eq  -- ^ @==@
    | Lt  -- ^ @<@
    | Leq -- ^ @<=@
    | Gt  -- ^ @>@
    | Geq -- ^ @>=@
    | Neq -- ^ @/=@
    deriving (Eq, Ord, Show, Generic, Bounded, Enum)

-- | Variable.
newtype Var = Var Text
    deriving (Eq, Ord, Show, Generic)

instance IsString Var where fromString = coerce . T.pack

-- | Smallest unit of data. Atoms unify variables and literals, and are what
-- functions take as arguments.
data Atom =
      AtomVar !Var
    | AtomLit !Literal
    deriving (Eq, Ord, Show, Generic)

-- | Constructors of algebraic data types.
newtype Constr = Constr Text
    deriving (Eq, Ord, Show, Generic)

instance IsString Constr where fromString = coerce . T.pack



--------------------------------------------------------------------------------
-- Lift instances
deriveLiftMany [ ''Program, ''Literal, ''LambdaForm, ''UpdateFlag, ''Rec
               , ''Expr, ''Alts, ''AlgebraicAlt, ''PrimitiveAlt, ''DefaultAlt
               , ''PrimOp, ''Atom ]

instance Lift NonDefaultAlts where
    lift NoNonDefaultAlts = [| NoNonDefaultAlts |]
    lift (AlgebraicAlts alts) =
        [| AlgebraicAlts (NonEmpty.fromList $(lift (toList alts))) |]
    lift (PrimitiveAlts alts) =
        [| PrimitiveAlts (NonEmpty.fromList $(lift (toList alts))) |]

instance Lift Binds where
    lift (Binds binds) = [| Binds (M.fromList $(lift (M.assocs binds))) |]

instance Lift Constr where
    lift (Constr con) = [| Constr (T.pack $(lift (T.unpack con))) |]

instance Lift Var where
    lift (Var var) = [| Var (T.pack $(lift (T.unpack var))) |]



--------------------------------------------------------------------------------
-- Pretty instances

semicolonTerminated :: [Doc] -> Doc
semicolonTerminated = align . vsep . punctuate (semicolon style ";")

instance Pretty Program where
    pretty (Program binds) = pretty binds

instance Pretty Binds where
    pretty (Binds bs) =
        (semicolonTerminated . map prettyBinding . M.assocs) bs
      where
        prettyBinding (var, lambda) =
            pretty var <+> "=" <+> pretty lambda

-- | Prettyprint a 'LambdaForm', given prettyprinters for the free variable
-- list.
--
-- Introduced so 'Stg.Machine.Types.Closure' can hijack it to display
-- the free value list differently.
prettyLambda
    :: ([Var] -> Doc) -- ^ Free variable list printer
    -> LambdaForm
    -> Doc
prettyLambda pprFree (LambdaForm free upd bound expr) =
    (prettyExp . prettyUpd . prettyBound . prettyFree) "\\"
  where
    prettyFree | null free = id
               | otherwise = (<> lparen <> pprFree free <> rparen)
    prettyUpd = (<+> case upd of Update   -> "=>"
                                 NoUpdate -> "->" )
    prettyBound | null bound = id
                | null free = (<> prettyList bound)
                | otherwise = (<+> prettyList bound)
    prettyExp = (<+> pretty expr)

instance Pretty LambdaForm where
    pretty = prettyLambda prettyList

instance Pretty Rec where
    pretty = \case
        NonRecursive -> ""
        Recursive    -> "rec"

instance Pretty Expr where
    pretty = \case
        Let rec binds expr ->
            let inBlock = indent 4 (keyword style "in" <+> pretty expr)
                bindingBlock = line <> indent 4 (
                    keyword style ("let" <> pretty rec) <+> pretty binds )
            in vsep [bindingBlock, inBlock]

        Case expr alts -> vsep [ hsep [ keyword style "case"
                                      , pretty expr
                                      , keyword style "of" ]
                               , indent 4 (align (pretty alts)) ]

        AppF var []   -> pretty var
        AppF var args -> pretty var <+> prettyList args

        AppC con []   -> pretty con
        AppC con args -> pretty con <+> prettyList args

        AppP op arg1 arg2 -> pretty op <+> pretty arg1 <+> pretty arg2

        Lit lit -> pretty lit

instance Pretty Alts where
    pretty (Alts NoNonDefaultAlts def) = pretty def
    pretty (Alts (AlgebraicAlts alts) def) =
        semicolonTerminated (map pretty (toList alts) <> [pretty def])
    pretty (Alts (PrimitiveAlts alts) def) =
        semicolonTerminated (map pretty (toList alts) <> [pretty def])

instance Pretty AlgebraicAlt where
    pretty (AlgebraicAlt con [] expr)
        = pretty con <+> "->" <+> pretty expr
    pretty (AlgebraicAlt con args expr)
        = pretty con <+> prettyList args <+> "->" <+> pretty expr

instance Pretty PrimitiveAlt where
    pretty (PrimitiveAlt lit expr) =
        pretty lit <+> "->" <+> pretty expr

instance Pretty DefaultAlt where
    pretty = \case
        DefaultNotBound expr  -> "default" <+> "->" <+> pretty expr
        DefaultBound var expr -> pretty var <+> "->" <+> pretty expr

instance Pretty Literal where
    pretty (Literal i) = prim style (integer i <> "#")

instance Pretty PrimOp where
    pretty op = prim style (case op of
        Add -> "+#"
        Sub -> "-#"
        Mul -> "*#"
        Div -> "/#"
        Mod -> "%#"
        Eq  -> "==#"
        Lt  -> "<#"
        Leq -> "<=#"
        Gt  -> ">#"
        Geq -> ">=#"
        Neq -> "/=#" )

instance Pretty Var where
    pretty (Var name) = variable style (string (T.unpack name))
    prettyList = hsep . map pretty

instance Pretty Atom where
    pretty = \case
        AtomVar var -> pretty var
        AtomLit lit -> pretty lit
    prettyList = hsep . map pretty

instance Pretty Constr where
    pretty (Constr name) = constructor style (string (T.unpack name))

instance NFData Program
instance NFData Binds
instance NFData LambdaForm
instance NFData UpdateFlag
instance NFData Rec
instance NFData Expr
instance NFData Alts
instance NFData NonDefaultAlts
instance NFData AlgebraicAlt
instance NFData PrimitiveAlt
instance NFData DefaultAlt
instance NFData Literal
instance NFData PrimOp
instance NFData Var
instance NFData Atom
instance NFData Constr
