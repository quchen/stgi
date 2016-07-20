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
--     @{free} \\n {bound} -> body@, which uses comma-separated lists. The
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
import           Data.List                    as L
import qualified Data.List.NonEmpty           as NonEmpty
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Monoid
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
skipToken :: StgParser a -> StgParser ()
skipToken = void . token

-- | A parser for an STG syntax element.
newtype StgParser ast = StgParser (Trifecta.Parser ast)
    deriving (CharParsing, Parsing, Alternative, Applicative, Functor, Monad)

instance TokenParsing StgParser where
    someSpace = skipMany (void (satisfy isSpace) <|> comment)

-- | Syntax rules for parsing variable-looking like identifiers.
varId :: IdentifierStyle StgParser
varId = IdentifierStyle
    { _styleName = "variable"
    , _styleStart = lower <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_'"
    , _styleReserved = ["let", "letrec", "in", "case", "of", "default"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier }

-- | Parse a variable identifier. Variables start with a lower-case letter or
-- @_@, followed by a string consisting of alphanumeric characters or @'@, @_@.
var :: StgParser Var
var = ident varId

-- | Skip a reserved variable identifier.
reserved :: Text -> StgParser ()
reserved = reserveText varId

-- | Parse a constructor identifier. Constructors follow the same naming
-- conventions as variables, but start with an upper-case character instead, and
-- may end with a @#@ symbol.
con :: StgParser Constr
con = highlight Constructor constructor <?> "constructor"
  where
    constructor = token (do
        start <- upper
        body  <- many (alphaNum <|> oneOf "_'")
        end   <- string "#" <|> pure ""
        (pure . Constr . T.pack) (start : body <> end) )

-- | Parse an STG program.
program :: StgParser Program
program = someSpace *> fmap Program binds <* eof <?> "STG program"

-- | Parse a collection of bindings, used by @let(rec)@ expressions and at the
-- top level of a program.
binds :: StgParser Binds
binds = bindings <?> "non-empty list of bindings"
  where
    bindings = fmap (Binds . M.fromList) (sepBy1 binding semi)
    binding = (,) <$> var <* symbol "=" <*> lambdaForm

comment :: StgParser ()
comment = skipToken (highlight Comment (lineComment <|> blockComment)) <?> ""
  where
    lineComment  = try (symbol "--") *> manyTill anyChar (char '\n')
    blockComment = try (symbol "{-") *> manyTill anyChar (try (symbol "-}"))

-- | Parse a lambda form, consisting of a list of free variables, and update
-- flag, a list of bound variables, and the function body.
lambdaForm :: StgParser LambdaForm
lambdaForm = lf >>= validateLambda <?> "lambda form"
  where
    lf :: StgParser LambdaForm
    lf = (\free bound upd body -> LambdaForm free upd bound body)
         <$  token (char '\\')
         <*> (parens (some var) <|> pure [])
         <*> many var
         <*> updateArrow
         <*> expr

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
    updateArrow :: StgParser UpdateFlag
    updateArrow = token (symbol "->" *> pure NoUpdate
                     <|> symbol "=>" *> pure Update
                     <?> "update arrow" )

-- | Parse an arrow token, @->@.
arrow :: StgParser ()
arrow = skipToken (symbol "->")

-- | Parse an expression, which can be
--
--   * let, @let(rec) ... in ...@
--   * case, @case ... of ...@
--   * function application, @f (...)@
--   * constructor application, @C (...)@
--   * primitive application, @p# (...)@
--   * literal, @1#@
expr :: StgParser Expr
expr = choice [let', case', appF, appC, appP, lit] <?> "expression"
  where
    let', case', appF, appC, appP, lit :: StgParser Expr

    let' = Let
        <$> (Recursive <$ reserved "letrec" <|> NonRecursive <$ reserved "let")
        <*> binds
        <*  reserved "in"
        <*> expr
        <?> "let(rec)"
    case' = Case
        <$  reserved "case"
        <*> (expr <?> "expression (as case scrutinee)")
        <*  reserved "of"
        <*> alts
        <?> "case expression"
    appF = AppF <$> var <*> many atom <?> "function application"
    appC = AppC <$> con <*> many atom <?> "constructor application"
    appP = AppP <$> primOp <*> atom <*> atom <?> "primitive function application"
    lit = Lit <$> literal <?> "literal expression"

-- | Parse the alternatives given in a @case@ expression.
alts :: StgParser Alts
alts = Alts
       <$> nonDefaultAlts
       <*> defaultAlt
       <?> "case alternatives"

-- | Parse an atom
atom :: StgParser Atom
atom = AtomVar <$> var
   <|> AtomLit <$> literal
   <?> "atom (variable or literal)"

-- | Parse a primitive operation.
--
-- @
-- +#
-- @
primOp :: StgParser PrimOp
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

-- | Parse an integer literal
--
-- @
-- 1#
-- -123#
-- @
literal :: StgParser Literal
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
nonDefaultAlts :: StgParser NonDefaultAlts
nonDefaultAlts = AlgebraicAlts . NonEmpty.fromList <$> some algebraicAlt
             <|> PrimitiveAlts . NonEmpty.fromList <$> some primitiveAlt
             <|> pure NoNonDefaultAlts
             <?> "non-default case alternatives"

-- | Parse a single algebraic alternative.
--
-- @
-- Cons x xs -> ...
-- @
algebraicAlt :: StgParser AlgebraicAlt
algebraicAlt = try (AlgebraicAlt <$> con)
           <*> (many var >>= disallowDuplicates)
           <*  arrow
           <*> expr
           <*  semi
           <?> "algebraic case alternative"
  where
    disallowDuplicates vars = case duplicates vars of
        [] -> pure vars
        dups ->
            let plural = case dups of [_] -> ""; _ -> "s"
                errMsg = "Duplicate variable" <> plural <> " in binding: "
                         <> L.intercalate ", " varNames
                varNames = map (\(Var v) -> T.unpack v) dups
            in fail errMsg
    duplicates = mapMaybe (\case (x:_:_) -> Just x; _ -> Nothing) . group . sort

-- | Parse a single primitive alternative, such as @1#@.
--
-- @
-- 1# -> ...
-- @
primitiveAlt :: StgParser PrimitiveAlt
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
defaultAlt :: StgParser DefaultAlt
defaultAlt = DefaultNotBound <$ reserved "default" <* arrow <*> expr
         <|> DefaultBound <$> var <* arrow <*> expr
         <?> "default alternative"
