{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | A parser for the STG language, modeled after the grammar given in the
-- description in the 1992 paper
-- <http://research.microsoft.com/apps/pubs/default.aspx?id=67083 (link)>
-- with a couple of differences to enhance usability:
--
--   * Function application uses no parentheses or commas like in Haskell
--     (@f x y z@), not with curly parentheses and commas like in the paper
--     (@f {x,y,z}@).
--   * Comment syntax like in Haskell: @-- inline@, @{- multiline -}@.
--   * Constructors may end with a @#@ to allow labelling primitive boxes
--     e.g. with @Int#@.
--   * A lambda's head is written @\\(free) bound -> body@, where @free@ and
--     @bound@ are space-separated variable lists, instead of the paper's
--     @(free) \\n (bound) -> body@, which uses comma-separated lists. The
--     update flag @\\u@ is signified using a double arrow @=>@ instead of the
--     normal arrow @->@.
module Stg.Parser.Parser (

    -- * General parsing
    parse,
    StgParser,

    -- * Parser rules
    program,
    binds,
    lambdaForm,
    expr,
    alts,
    nonDefaultAlts,
    algebraicAlt,
    primitiveAlt,
    defaultAlt,
    literal,
    primOp,
    atom,
    var,
    con,
) where



import           Control.Applicative
import           Control.Monad
import           Data.Char                    (isSpace)
import qualified Data.List.NonEmpty           as NonEmpty
import qualified Data.Map.Strict              as M
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.Parser.Token.Highlight
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta                as Trifecta

import Stg.Language



-- | Parse STG source using a user-specified parser. To parse a full program,
-- use @'parse' 'program'@.
--
-- >>> parse program "id = \\x -> x"
-- Right (Program (Binds [(Var "id",LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") []))]))
parse :: StgParser ast -> Text -> Either Doc ast
parse (StgParser p) input = case parseString (whiteSpace *> p <* eof) mempty (T.unpack input) of
    Success a -> Right a
    Failure e -> Left e

-- | Skip a certain token. Useful to consume, but not otherwise use, certain
-- tokens.
skipToken :: TokenParsing parser => parser a -> parser ()
skipToken = void . token

-- | A parser for an STG syntax element.
newtype StgParser ast = StgParser (Trifecta.Parser ast)
    deriving (CharParsing, Parsing, Alternative, Applicative, Functor, Monad)

instance TokenParsing StgParser where
    someSpace = skipMany (void (satisfy isSpace) <|> comment)

-- | Syntax rules for parsing variable-looking like identifiers.
varId :: TokenParsing parser => IdentifierStyle parser
varId = IdentifierStyle
    { _styleName = "variable"
    , _styleStart = lower <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_'"
    , _styleReserved = ["let", "letrec", "in", "case", "of", "default", "_"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier }

-- | Parse a variable identifier. Variables start with a lower-case letter or
-- @_@, followed by a string consisting of alphanumeric characters or @'@, @_@.
var :: (Monad parser, TokenParsing parser) => parser Var
var = ident varId

-- | Skip a reserved variable identifier.
reserved :: (Monad parser, TokenParsing parser) => Text -> parser ()
reserved = reserveText varId

-- | Syntax rules for parsing constructor-looking like identifiers.
conId :: TokenParsing parser => IdentifierStyle parser
conId = IdentifierStyle
    { _styleName = "constructor"
    , _styleStart = upper
    , _styleLetter = alphaNum <|> oneOf "_'#" -- TODO: allow '#' only at the end
    , _styleReserved = []
    , _styleHighlight = Constructor
    , _styleReservedHighlight = ReservedConstructor }

-- | Parse a constructor identifier. Constructors follow the same naming
-- conventions as variables, but start with an upper-case character instead, and
-- may end with a @#@ symbol.
con :: (Monad parser, TokenParsing parser) => parser Constr
con = ident conId

-- | Parse an STG program.
program :: (Monad parser, TokenParsing parser) => parser Program
program = someSpace *> fmap Program binds <* eof

-- | Parse a collection of bindings, used by @let(rec)@ expressions and at the
-- top level of a program.
binds :: (Monad parser, TokenParsing parser) => parser Binds
binds = fmap (Binds . M.fromList) (sepBy1 binding semi)
  where
    binding :: (Monad parser, TokenParsing parser) => parser (Var, LambdaForm)
    binding = (,) <$> var <* symbol "=" <*> lambdaForm

comment :: TokenParsing parser => parser ()
comment = skipToken (highlight Comment (lineComment <|> blockComment)) <?> ""
  where
    lineComment  = try (symbol "--") *> manyTill anyChar (char '\n')
    blockComment = try (symbol "{-") *> manyTill anyChar (try (symbol "-}"))

-- | Parse a lambda form, consisting of a list of free variables, and update
-- flag, a list of bound variables, and the function body.
lambdaForm :: (Monad parser, TokenParsing parser) => parser LambdaForm
lambdaForm = lf >>= validateLambda
  where
    lf :: (Monad parser, TokenParsing parser) => parser LambdaForm
    lf = (\free bound upd body -> LambdaForm free upd bound body)
         <$  token (char '\\')
         <*> (parens (some var) <|> pure [])
         <*> many var
         <*> updateArrow
         <*> expr
         <?> "lambda form"

    validateLambda = \case
        LambdaForm _ Update [] AppC{} ->
           fail "Standard constructors are never updatable"
        LambdaForm _ Update (_:_) _ ->
           fail "Lambda forms with non-empty argument lists are never updatable"
        LambdaForm _ _ _ Lit{} ->
           fail "No lambda form has primitive type like 1#;\
                \ primitives must be boxed, e.g. Int# (1#)"
        LambdaForm _ _ _ AppP{} ->
           fail "No lambda form has primitive type like \"+# a b\";\
                \ only \"case\" can evaluate them"
        x -> pure x

    -- Parse an update flag arrow. @->@ means no update, @=>@ update.
    updateArrow :: (Monad parser, TokenParsing parser) => parser UpdateFlag
    updateArrow = token (symbol "->" *> pure NoUpdate
                     <|> symbol "=>" *> pure Update
                     <?> "Update arrow" )

-- | Parse an arrow token, @->@.
arrow :: TokenParsing parser => parser ()
arrow = skipToken (symbol "->")

-- | Parse an expression, which can be
--
--   * let, @let(rec) ... in ...@
--   * case, @case ... of ...@
--   * function application, @f (...)@
--   * constructor application, @C (...)@
--   * primitive application, @p# (...)@
--   * literal, @1#@
expr :: (Monad parser, TokenParsing parser) => parser Expr
expr = choice [let', case', appF, appC, appP, lit] <?> "expression"
  where
    letHead
        :: (Monad parser, TokenParsing parser)
        => parser (Binds -> Expr -> Expr)
    let', case', appF, appC, appP, lit
        :: (Monad parser, TokenParsing parser)
        => parser Expr

    letHead = reserved "letrec" *> pure (Let Recursive)
          <|> reserved "let"    *> pure (Let NonRecursive)
    let' = letHead
        <*> (binds <?> "list of free variables")
        <*  reserved "in"
        <*> (expr <?> "let(rec) expression")
        <?> "let"
    case' = Case
        <$  reserved "case"
        <*> (expr <?> "case scrutinee")
        <*  reserved "of"
        <*> alts
        <?> "case expression"
    appF = AppF <$> var <*> many atom <?> "function application"
    appC = AppC <$> con <*> many atom <?> "constructor application"
    appP = AppP <$> primOp <*> atom <*> atom <?> "primitive function application"
    lit = Lit <$> literal <?> "literal expression"

-- | Parse the alternatives given in a @case@ expression.
alts :: (Monad parser, TokenParsing parser) => parser Alts
alts = Alts
       <$> nonDefaultAlts
       <*> defaultAlt
       <?> "case alternatives"

atom :: (Monad parser, TokenParsing parser) => parser Atom
atom = AtomVar <$> var
   <|> AtomLit <$> literal
   <?> "atom"


-- | Parse a primitive operation.
--
-- @
-- +#
-- @
primOp :: TokenParsing parser => parser PrimOp
primOp = choice ops <?> "primitive function"
  where
    ops = [ "+"  ~> Add
          , "-"  ~> Sub
          , "*"  ~> Mul
          , "/"  ~> Div
          , "%"  ~> Mod
          , "<"  ~> Lt
          , "<=" ~> Leq
          , "==" ~> Eq
          , "/=" ~> Neq
          , ">=" ~> Geq
          , ">"  ~> Gt ]
    op ~> val = token (try (string op <* char '#')) *> pure val

literal :: TokenParsing parser => parser Literal
literal = token (Literal <$> integer' <* char '#') <?> "integer literal"


-- | Parse non-default alternatives. The list of alternatives can be either
-- empty, all algebraic, or all primitive.
--
-- @
-- Nil -> ...
-- Cons x xs -> ...
-- @
--
-- @
-- 1# -> ...
-- 2# -> ...
-- @
nonDefaultAlts :: (Monad parser, TokenParsing parser) => parser NonDefaultAlts
nonDefaultAlts = AlgebraicAlts . NonEmpty.fromList <$> some algebraicAlt
             <|> PrimitiveAlts . NonEmpty.fromList <$> some primitiveAlt
             <|> pure NoNonDefaultAlts
             <?> "non-default case alternatives"

-- | Parse a single algebraic alternative.
--
-- @
-- Cons x xs -> ...
-- @
algebraicAlt :: (Monad parser, TokenParsing parser) => parser AlgebraicAlt
algebraicAlt = try (AlgebraicAlt <$> con)
                <*> many var
                <*  arrow
                <*> expr
                <*  semi
                <?> "algebraic case alternative"

-- | Parse a single primitive alternative, such as @1#@.
--
-- @
-- 1# -> ...
-- @
primitiveAlt :: (Monad parser, TokenParsing parser) => parser PrimitiveAlt
primitiveAlt = try (PrimitiveAlt <$> literal) <* arrow <*> expr <* semi
    <?> "primitive case alternative"

-- | Parse the default alternative, taken if none of the other alternatives
-- in a @case@ expression match.
--
-- @
-- default -> ...
-- @
--
-- @
-- v -> ...
-- @
defaultAlt :: (Monad parser, TokenParsing parser) => parser DefaultAlt
defaultAlt = DefaultNotBound <$ reserved "default" <* arrow <*> expr
         <|> DefaultBound <$> var <* arrow <*> expr
         <?> "default alternative"
