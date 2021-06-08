{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

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
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import qualified Data.Semigroup            as Semigroup
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           GHC.Exts
import           GHC.Generics
import           Language.Haskell.TH.Syntax (Lift(liftTyped))

import Stg.Language.Prettyprint



-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Stg.Parser.QuasiQuoter


-- | An STG 'Program' is the unit that can be loaded by the STG machine. It
-- consists of a set of bindings.
newtype Program = Program Binds
    deriving (Eq, Ord, Show, Generic, Lift)

-- | __Right-biased union__ of the contained bindings. This makes for a poor man's
-- module system by appending multiple, potentially partially incomplete,
-- 'Programs' to each other.
--
-- @
-- 'Stg.Prelude.map' <> 'Stg.Prelude.filter' <> ['Stg.Parser.QuasiQuoter.stg'| … actual source … |]
-- @
instance Monoid Program where
    mempty = Program mempty
    mappend = (Semigroup.<>)

instance Semigroup.Semigroup Program where
    Program x <> Program y = Program (x <> y)

-- | Bindings are collections of lambda forms, indexed over variables.
--
-- They exist at the top level, or as part of a let(rec) binding.
newtype Binds = Binds (Map Var LambdaForm)
    deriving (Eq, Ord, Generic)

-- | __Right-biased__ union. See 'Monoid' 'Program' for further information.
instance Monoid Binds where
    mempty = Binds mempty
    mappend = (Semigroup.<>)

instance Semigroup.Semigroup Binds where
    Binds x <> Binds y = Binds (x <> y)

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
    deriving (Eq, Ord, Show, Generic, Lift)

-- | Possible classification of lambda forms.
data LambdaType =
      LambdaCon   -- ^ Data constructor ('AppC' as body)
    | LambdaFun   -- ^ Function (lambda with non-empty argument list)
    | LambdaThunk -- ^ Thunk (everything else)
    deriving (Eq, Ord, Show)

instance PrettyStgi LambdaType where
    prettyStgi = \case
        LambdaCon   -> "Con"
        LambdaFun   -> "Fun"
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
    deriving (Eq, Ord, Show, Generic, Enum, Bounded, Lift)

-- | Distinguishes @let@ from @letrec@.
data Rec =
      NonRecursive -- ^ Bindings have no access to each other
    | Recursive -- ^ Bindings can be given to each other as free variables
    deriving (Eq, Ord, Show, Generic, Enum, Bounded, Lift)

-- | An expression in the STG language.
data Expr =
      Let !Rec !Binds !Expr    -- ^ Let expression @let(rec) ... in ...@
    | Case !Expr !Alts         -- ^ Case expression @case ... of ... x -> y@
    | AppF !Var ![Atom]        -- ^ Function application @f x y z@
    | AppC !Constr ![Atom]     -- ^ Saturated constructor application @Just a@
    | AppP !PrimOp !Atom !Atom -- ^ Primitive function application @+# 1# 2#@
    | LitE !Literal            -- ^ Literal expression @1#@
    deriving (Eq, Ord, Show, Generic, Lift)

-- | List of possible alternatives in a 'Case' expression.
--
-- The list of alts has to be homogeneous. This is not ensured by the type
-- system, and should be handled by the parser instead.
data Alts = Alts !NonDefaultAlts !DefaultAlt
    deriving (Eq, Ord, Show, Generic, Lift)

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
    deriving (Eq, Ord, Show, Generic, Lift)

-- | As in @1#@, @2#@, @3#@
data PrimitiveAlt = PrimitiveAlt !Literal !Expr
    deriving (Eq, Ord, Show, Generic, Lift)

-- | If no viable alternative is found in a pattern match, use a 'DefaultAlt'
-- as fallback.
data DefaultAlt =
       DefaultNotBound !Expr
     | DefaultBound !Var !Expr
    deriving (Eq, Ord, Show, Generic, Lift)

-- | Literals are the basis of primitive operations.
newtype Literal = Literal Integer
    deriving (Eq, Ord, Show, Generic, Lift)

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
    deriving (Eq, Ord, Show, Generic, Bounded, Enum, Lift)

-- | Variable.
newtype Var = Var Text
    deriving (Eq, Ord, Show, Generic)

instance IsString Var where fromString = coerce . T.pack

-- | Smallest unit of data. Atoms unify variables and literals, and are what
-- functions take as arguments.
data Atom =
      AtomVar !Var
    | AtomLit !Literal
    deriving (Eq, Ord, Show, Generic, Lift)

-- | Constructors of algebraic data types.
newtype Constr = Constr Text
    deriving (Eq, Ord, Show, Generic)

instance IsString Constr where fromString = coerce . T.pack

--------------------------------------------------------------------------------
-- Lift instances
instance Lift NonDefaultAlts where
    liftTyped NoNonDefaultAlts = [|| NoNonDefaultAlts ||]
    liftTyped (AlgebraicAlts alts) =
        [|| AlgebraicAlts (NonEmpty.fromList $$(liftTyped (toList alts))) ||]
    liftTyped (PrimitiveAlts alts) =
        [|| PrimitiveAlts (NonEmpty.fromList $$(liftTyped (toList alts))) ||]

instance Lift Binds where
    liftTyped (Binds binds) = [|| Binds (M.fromList $$(liftTyped (M.assocs binds))) ||]

instance Lift Constr where
    liftTyped (Constr con) = [|| Constr (T.pack $$(liftTyped (T.unpack con))) ||]

instance Lift Var where
    liftTyped (Var var) = [|| Var (T.pack $$(liftTyped (T.unpack var))) ||]

--------------------------------------------------------------------------------
-- Pretty instances

semicolonTerminated :: [Doc StgiAnn] -> Doc StgiAnn
semicolonTerminated = align . vsep . punctuate (annotate (AstAnn Semicolon) ";")

instance PrettyStgi Program where
    prettyStgi (Program binds) = prettyStgi binds

instance PrettyStgi Binds where
    prettyStgi (Binds bs) =
        (semicolonTerminated . map prettyBinding . M.assocs) bs
      where
        prettyBinding (var, lambda) =
            prettyStgi var <+> "=" <+> prettyStgi lambda

-- | Prettyprint a 'LambdaForm', given prettyprinters for the free variable
-- list.
--
-- Introduced so 'Stg.Machine.Types.Closure' can hijack it to display
-- the free value list differently.
prettyLambda
    :: ([Var] -> Doc StgiAnn) -- ^ Free variable list printer
    -> LambdaForm
    -> Doc StgiAnn
prettyLambda pprFree (LambdaForm free upd bound expr) =
    (prettyExp . prettyUpd . prettyBound . prettyFree) "\\"
  where
    prettyFree | null free = id
               | otherwise = (<> lparen <> pprFree free <> rparen)
    prettyUpd = (<+> case upd of Update   -> "=>"
                                 NoUpdate -> "->" )
    prettyBound | null bound = id
                | null free = (<> hsep (map prettyStgi bound))
                | otherwise = (<+> hsep (map prettyStgi bound))
    prettyExp = (<+> prettyStgi expr)

instance PrettyStgi LambdaForm where
    prettyStgi = prettyLambda (hsep . map prettyStgi)

instance PrettyStgi Rec where
    prettyStgi = \case
        NonRecursive -> ""
        Recursive    -> "rec"

instance PrettyStgi Expr where
    prettyStgi = \case
        Let rec binds expr ->
            let inBlock = indent 4 (annotate (AstAnn Keyword) "in" <+> prettyStgi expr)
                bindingBlock = line <> indent 4 (
                    annotate (AstAnn Keyword) ("let" <> prettyStgi rec) <+> prettyStgi binds )
            in vsep [bindingBlock, inBlock]

        Case expr alts -> vsep [ hsep [ annotate (AstAnn Keyword) "case"
                                      , prettyStgi expr
                                      , annotate (AstAnn Keyword) "of" ]
                               , indent 4 (align (prettyStgi alts)) ]

        AppF var []   -> prettyStgi var
        AppF var args -> prettyStgi var <+> hsep (map prettyStgi args)

        AppC con []   -> prettyStgi con
        AppC con args -> prettyStgi con <+> hsep (map prettyStgi args)

        AppP op arg1 arg2 -> prettyStgi op <+> prettyStgi arg1 <+> prettyStgi arg2

        LitE lit -> prettyStgi lit

instance PrettyStgi Alts where
    prettyStgi (Alts NoNonDefaultAlts def) = prettyStgi def
    prettyStgi (Alts (AlgebraicAlts alts) def) =
        semicolonTerminated (map prettyStgi (toList alts) <> [prettyStgi def])
    prettyStgi (Alts (PrimitiveAlts alts) def) =
        semicolonTerminated (map prettyStgi (toList alts) <> [prettyStgi def])

instance PrettyStgi AlgebraicAlt where
    prettyStgi (AlgebraicAlt con [] expr)
        = prettyStgi con <+> "->" <+> prettyStgi expr
    prettyStgi (AlgebraicAlt con args expr)
        = prettyStgi con <+> hsep (map prettyStgi args) <+> "->" <+> prettyStgi expr

instance PrettyStgi PrimitiveAlt where
    prettyStgi (PrimitiveAlt lit expr) =
        prettyStgi lit <+> "->" <+> prettyStgi expr

instance PrettyStgi DefaultAlt where
    prettyStgi = \case
        DefaultNotBound expr  -> "default" <+> "->" <+> prettyStgi expr
        DefaultBound var expr -> prettyStgi var <+> "->" <+> prettyStgi expr

instance PrettyStgi Literal where
    prettyStgi (Literal i) = annotate (AstAnn Prim) (pretty i <> "#")

instance PrettyStgi PrimOp where
    prettyStgi op = annotate (AstAnn Prim) (case op of
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

instance PrettyStgi Var where
    prettyStgi (Var name) = annotate (AstAnn Variable) (pretty name)

instance PrettyStgi Atom where
    prettyStgi = \case
        AtomVar var -> prettyStgi var
        AtomLit lit -> prettyStgi lit

instance PrettyStgi Constr where
    prettyStgi (Constr name) = annotate (AstAnn Constructor) (pretty name)

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
