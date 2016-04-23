-- | A parser for the STG language, modeled after the grammar given in the
-- description in the 1992 paper
-- <http://research.microsoft.com/apps/pubs/default.aspx?id=67083 (link)>
-- with a couple of minor differences:
--
--   * Values are represented by function application to an empty argument list,
--     @x ()@, as opposed to having no argument list at all in the paper.
--   * parentheses @()@ instead of curly braces @{}@
--   * Comment syntax like in Haskell
--   * Constructors can end with a @#@ to allow labelling primitive boxes
--     e.g. with @Int#@.
module Stg.Parser.Parser (
    -- Parsing functions
    parse,

    -- Parser values
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
    vars,
    atoms,
    atom,
    varTok,
    conTok,
) where



import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import qualified Data.Map              as M
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Megaparsec       ((<?>))
import qualified Text.Megaparsec       as P
import qualified Text.Megaparsec.Char  as C
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

import           Stg.Language

-- $setup
-- >>> :set -XOverloadedStrings



--------------------------------------------------------------------------------
-- * Convenience

-- | Parse STG source using a user-specified parser. To parse a full program,
-- use @'parse' 'program'@.
--
-- >>> parse program "id = () \\n (x) -> x ()"
-- Right (Program (Binds [(Var "id",LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") []))]))
parse :: Parser ast -> Text -> Either Text ast
parse p input = first (T.pack . show) parseResult
  where
    parseResult = P.runParser (spaceConsumer *> p <* P.eof)
                              "input"
                              input



--------------------------------------------------------------------------------
-- * Lexing

-- | Parser that skips spaces and comments.
--
--   * Line comments start with @--@
--   * Block comments are enclosed in @{- ... -}@
spaceConsumer :: Parser ()
spaceConsumer = L.space (P.some P.spaceChar *> pure ())
                        (L.skipLineComment "--")
                        (L.skipBlockComment "{-" "-}")

-- | Apply a parser, and skip whitespace following it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Parse a certain chain of characters.
symbol :: String -> Parser ()
symbol s = void (lexeme (C.string s)) <?> s

-- | Semicolon character, separating bindings and alternatives from
-- each other.
semicolonTok :: Parser ()
semicolonTok = symbol ";"

-- | Parse the comma token, separating arguments from each other.
commaTok :: Parser ()
commaTok = symbol ","

-- | Parse a @let@ or @letrec@ token.
letTok :: Parser (Binds -> Expr -> Expr)
letTok = P.try (lexeme (C.string "let"    *> C.spaceChar) *> pure (Let NonRecursive))
     <|> P.try (lexeme (C.string "letrec" *> C.spaceChar) *> pure (Let Recursive))

-- | Parse the @in@ token corresponding to a @let(rec)@.
inTok :: Parser ()
inTok = symbol "in"

-- | Parse the @case@ token.
caseTok :: Parser (Expr -> Alts -> Expr)
caseTok = lexeme (C.string "case" *> C.spaceChar) *> pure Case

-- | Parse the @of@ corresponding to a @case@.
ofTok :: Parser ()
ofTok = symbol "of"

-- | Parse the assignment operator @=@.
assignTok :: Parser ()
assignTok = symbol "="

-- | Parse a variable name. Variables start with a lower-case letter, followed
-- by a string consisting of alphanumeric characters or @'@, @_@.
varTok :: Parser Var
varTok = lexeme p <?> "variable"
  where
    p = liftA2 (\x xs -> Var (T.pack (x:xs)))
               P.lowerChar
               (P.many (P.alphaNumChar <|> P.oneOf "\'_"))

-- | Parse a constructor name. Constructors follow the same naming conventions
-- as variables, but start with an upper-case character instead, and may
-- end with a @#@ symbol.
conTok :: Parser Constr
conTok = lexeme p <?> "constructor"
  where
    p = liftA3 (\x xs hash -> Constr (T.pack (x:xs ++ hash)))
               P.upperChar
               (P.many (P.alphaNumChar <|> P.oneOf "\'_"))
               (P.option "" (P.string "#"))

-- | Parse the @default@ token, used for alternatives that always match.
defNotBoundTok :: Parser (Expr -> DefaultAlt)
defNotBoundTok = symbol "default" *> pure DefaultNotBound

-- | Parse the arrow token, used in @case@ expression and lambda forms.
arrowTok :: Parser ()
arrowTok = symbol "->"

-- | Parse the hash token, used for primitive values and operations.
hashTok :: Parser ()
hashTok = symbol "#"

-- | Parse an opening parenthesis, used for argument lists.
openParenthesisTok :: Parser ()
openParenthesisTok = symbol "("

-- | Parse a closing parenthesis, used for argument lists.
closeParenthesisTok :: Parser ()
closeParenthesisTok = symbol ")"

-- | Given a parser @p@, @parenthesized p@ parses it enclosed in parentheses.
parenthesized :: Parser a -> Parser a
parenthesized = P.between openParenthesisTok closeParenthesisTok

-- | Parse an update flag, used in lambda forms to influence their evaluation
-- behaviour.
updateFlagTok :: Parser UpdateFlag
updateFlagTok = lexeme (P.char '\\' *> flag) <?> help
  where
    flag = C.char 'u' *> pure Update <|> C.char 'n' *> pure NoUpdate
    help = "\\u (update), \\n (no update)"

-- | Parse an integer, possibly with a sign.
signedIntegerTok :: Parser Integer
signedIntegerTok = L.signed spaceConsumer L.integer



--------------------------------------------------------------------------------
-- * Parsing

-- | Parse an STG program.
program :: Parser Program
program = spaceConsumer *> fmap Program binds <* P.eof

-- | Parse a collection of bindings, used by @let(rec)@ expressions and at the
-- top level of a program.
binds :: Parser Binds
binds = fmap (Binds . M.fromList) (P.sepBy binding semicolonTok)
  where
    binding :: Parser (Var, LambdaForm)
    binding = (,) <$> varTok <* assignTok <*> lambdaForm

-- | Parse a lambda form, consisting of a list of free variables, and update
-- flag, a list of bound variables, and the function body.
lambdaForm :: Parser LambdaForm
lambdaForm = lf >>= validateLambda
  where
    lf = LambdaForm
         <$> vars
         <*> updateFlagTok
         <*> vars
         <*  arrowTok
         <*> expr
         <?> "lambda form"
    validateLambda (LambdaForm _ Update (_:_) _) =
        fail "Lambda forms with non-empty argument lists are never updatable"
    validateLambda x = pure x

-- | Parse an expression, which can be
--
--   * let, @let(rec) ... in ...@
--   * case, @case ... of ...@
--   * function application, @f (...)@
--   * constructor application, @C (...)@
--   * primitive application, @p# (...)@
--   * literal, @1#@
expr :: Parser Expr
expr = P.choice [let', case', appF, appC, appP, lit]
  where
    let' = P.try letTok
       <*> (binds <?> "list of free variables")
       <*  inTok
       <*> (expr <?> "let body")
       <?> "let"
    case' = P.try caseTok
        <*> (expr <?> "case scrutinee")
        <*  ofTok
        <*> alts
        <?> "case"
    appF = AppF
        <$> varTok
        <*> atoms
        <?> "function application"
    appC = AppC
        <$> conTok
        <*> atoms
        <?> "constructor application"
    appP = AppP
        <$> primOp
        <*> atom
        <*> atom
        <?> "primitive function application"
    lit = Lit
        <$> literal
        <?> "literal"

-- | Parse the alternatives given in a @case@ expression.
alts :: Parser Alts
alts = Alts <$> nonDefaultAlts <*> defaultAlt
   <?> "case alternatives"

-- | Parse multiple non-default alternatives. The list of alternatives can
-- be either all algebraic or all primitive.
--
-- @
-- Nil () -> ...
-- Cons (x,xs) -> ...
-- @
--
-- @
-- 1# -> ...
-- 2# -> ...
-- @
nonDefaultAlts :: Parser [Alt]
nonDefaultAlts = p <|> pure [] <?> help
  where
    p = do
        firstAlt <- algebraicAlt <|> primitiveAlt
        rest <- case firstAlt of
            AlgebraicAlt{} -> many algebraicAlt
            PrimitiveAlt{} -> many primitiveAlt
        pure (firstAlt:rest)
    help = "non-default case alternatives"

-- | Parse a single algebraic alternative.
--
-- @
-- Cons (x,xs) -> ...
-- @
algebraicAlt :: Parser Alt
algebraicAlt = P.try (AlgebraicAlt <$> conTok <*> vars) <* arrowTok <*> expr <* semicolonTok
    <?> "algebraic case alternative"

-- | Parse a single primitive alternative, such as @1#@.
--
-- @
-- 1# -> ...
-- @
primitiveAlt :: Parser Alt
primitiveAlt = P.try (PrimitiveAlt <$> literal) <* arrowTok <*> expr <* semicolonTok
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
defaultAlt :: Parser DefaultAlt
defaultAlt = P.try defNotBoundTok <* arrowTok <*> expr
         <|> DefaultBound <$> varTok <* arrowTok <*> expr
         <?> "default alternative"

-- | Parse a literal.
--
-- @
-- 1#
-- @
literal :: Parser Literal
literal = (Literal . fromInteger) <$> signedIntegerTok <* hashTok
    <?> "integer literal"

-- | Parse a primitive operation.
--
-- @
-- +#
-- @
primOp :: Parser PrimOp
primOp = P.choice ops <?> "primitive function"
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
    op ~> val = P.try (P.string op <* hashTok) *> pure val

-- | Parse a number of variables. enclosed in parentheses, and separated by
-- commas. Used in lambda forms.
--
-- @
-- (f, x, y)
-- @
vars :: Parser [Var]
vars = parenthesized (P.sepBy varTok commaTok)
    <?> "variables"

-- | Parse a number of atoms. enclosed in parentheses, and separated by
-- commas. Used in function and constructor applications.
--
-- @
-- (f, x, 1#)
-- @
atoms :: Parser [Atom]
atoms = parenthesized (P.sepBy atom commaTok)
    <?> "atoms"

-- | Parse an atom, which can be either a variable or a literal.
atom :: Parser Atom
atom = AtomVar <$> varTok
   <|> AtomLit <$> literal
   <?> "atom"
