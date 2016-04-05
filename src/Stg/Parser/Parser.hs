-- | A parser for the STG language, modeled after the decription in the 1992
-- paper with a couple of minor differences:
--
--   * () instead of {}
--   * Comment syntax like in Haskell
module Stg.Parser.Parser where



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

--------------------------------------------------------------------------------
-- Lexing

spaceConsumer :: Parser ()
spaceConsumer = L.space (P.some P.spaceChar *> pure ())
                        (L.skipLineComment "--")
                        (L.skipBlockComment "{-" "-}")

symbol :: String -> Parser ()
symbol = void . L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

semicolonTok :: Parser ()
semicolonTok = symbol ";"

commaTok :: Parser ()
commaTok = symbol ";"

letTok :: Parser (Binds -> Expr -> Expr)
letTok = P.try (C.string "let"    *> spaceConsumer *> pure (Let NonRecursive))
     <|> P.try (C.string "letrec" *> spaceConsumer *> pure (Let Recursive))

inTok :: Parser ()
inTok = symbol "in"

caseTok :: Parser (Expr -> Alts -> Expr)
caseTok = symbol "case" *> pure Case

ofTok :: Parser ()
ofTok = symbol "of"

assignTok :: Parser ()
assignTok = symbol "="

varTok :: Parser Var
varTok = lexeme p <?> "variable"
  where
    p = liftA2 (\x xs -> Var (T.pack (x:xs)))
                P.lowerChar
                (many P.alphaNumChar)

conTok :: Parser Constr
conTok = lexeme p <?> "constructor"
  where
    p = liftA2 (\_x _xs -> ConstrPlaceholder)
                P.upperChar
                (many P.alphaNumChar)

defNotBoundTok :: Parser (Expr -> DefaultAlt)
defNotBoundTok = symbol "default" *> pure DefaultNotBound

arrowTok :: Parser ()
arrowTok = symbol "->"

hashTok :: Parser ()
hashTok = symbol "#"

openParenthesis :: Parser ()
openParenthesis = symbol "("

closeParenthesis :: Parser ()
closeParenthesis = symbol ")"

parenthesized :: Parser a -> Parser a
parenthesized = P.between openParenthesis closeParenthesis


--------------------------------------------------------------------------------
-- Parsing

parse :: Text -> Either String Program
parse = first show . P.runParser stgLanguage "(string)"

stgLanguage :: Parser Program
stgLanguage = fmap Program binds

binds :: Parser Binds
binds = fmap (Binds . M.fromList)
             (P.sepBy binding semicolonTok)

binding :: Parser (Var, LambdaForm)
binding = (,) <$> varTok <* assignTok <*> lambdaForm

lambdaForm :: Parser LambdaForm
lambdaForm = LambdaForm
         <$> vars
         <*> (P.char '\\' *> updateFlag)
         <*> vars
         <*  arrowTok
         <*> expr

updateFlag :: Parser UpdateFlag
updateFlag = p <?> help
  where
    p = symbol "u" *> pure Update <|> symbol "n" *> pure NoUpdate
    help = "u (update), n (no update)"

expr :: Parser Expr
expr = P.choice [let', case', appF, appC, appP, lit]
  where
    let'  = letTok <*> binds <* inTok <*> expr
    case' = caseTok <*> expr <* ofTok <*> alts
    appF  = AppF <$> varTok <*> atoms
    appC  = AppC <$> conTok <*> atoms
    appP  = AppP <$> primOp <*> atom <*> atom
    lit   = Lit <$> literal

alts :: Parser Alts
alts = AlgebraicAlts <$> algebraicAlts
   <|> PrimitiveAlts <$> primitiveAlts

algebraicAlts :: Parser AAlts
algebraicAlts = AAlts
            <$> P.sepBy aAlt semicolonTok
            <*  semicolonTok
            <*> def

primitiveAlts :: Parser PAlts
primitiveAlts = PAlts
            <$> P.sepBy pAlt semicolonTok
            <*  semicolonTok
            <*> def

aAlt :: Parser AAlt
aAlt = AAlt <$> conTok <*> vars <*> expr

pAlt :: Parser PAlt
pAlt = PAlt <$> literal <*> expr

def :: Parser DefaultAlt
def = P.try defNotBoundTok <*> expr
  <|> DefaultBound         <$> varTok <*> expr

literal :: Parser Literal
literal = Literal . fromInteger <$> P.try L.integer <* hashTok

primOp :: Parser PrimOp
primOp = P.choice choices <* hashTok
  where
    choices = [ P.char '+' *> pure Add
              , P.char '-' *> pure Sub
              , P.char '*' *> pure Mul
              , P.char '/' *> pure Div
              , P.char '%' *> pure Mod ]

vars :: Parser [Var]
vars = parenthesized (P.sepBy varTok commaTok)

atoms :: Parser [Atom]
atoms = parenthesized (P.sepBy atom commaTok)

atom :: Parser Atom
atom = AtomVar <$> varTok
   <|> AtomLit <$> literal
