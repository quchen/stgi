module Stg.Parser (parse) where



import           Control.Applicative
import           Data.Bifunctor
import qualified Data.Map              as M
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Text.Megaparsec       as P
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

import           Stg.Language

--------------------------------------------------------------------------------
-- Lexing

space :: Parser ()
space = L.space P.space
                (L.skipLineComment "--")
                (L.skipBlockComment "{-" "-}")

symbol :: String -> Parser String
symbol = L.symbol space

semicolonTok :: Parser ()
semicolonTok = symbol ";" *> pure ()

commaTok :: Parser ()
commaTok = symbol ";" *> pure ()

letTok :: Parser (Binds -> Expr -> Expr)
letTok = symbol "let" *> pure (Let NonRecursive)

letrecTok :: Parser (Binds -> Expr -> Expr)
letrecTok = symbol "letrec" *> pure (Let Recursive)

inTok :: Parser ()
inTok = symbol "in" *> pure ()

caseTok :: Parser (Expr -> Alts -> Expr)
caseTok = symbol "case" *> pure Case

ofTok :: Parser ()
ofTok = symbol "of" *> pure ()

varTok :: Parser Var
varTok = liftA2 (\x xs -> Var (T.pack (x:xs)))
                P.lowerChar
                (many P.alphaNumChar)

conTok :: Parser Constr
conTok = liftA2 (\_x _xs -> ConstrPlaceholder)
                P.upperChar
                (many P.alphaNumChar)

defNotBoundTok :: Parser (Expr -> DefaultAlt)
defNotBoundTok = symbol "default" *> pure DefaultNotBound

arrowTok :: Parser ()
arrowTok = symbol "->" *> pure ()

hashTok :: Parser ()
hashTok = symbol "#" *> pure ()

curlied :: Parser a -> Parser a
curlied = P.between (symbol "{") (symbol "}")


--------------------------------------------------------------------------------
-- Parsing

parse :: Text -> Either String Program
parse = first show . P.runParser stgLanguage "(string)"

stgLanguage :: Parser Program
stgLanguage = fmap Program binds

binds :: Parser Binds
binds = fmap (Binds . M.fromList) (P.sepBy binding semicolonTok)

binding :: Parser (Var, LambdaForm)
binding = liftA2 (,) varTok lambdaForm

lambdaForm :: Parser LambdaForm
lambdaForm = LambdaForm
         <$> vars
         <*> (P.char '\\' *> updateFlag)
         <*> vars
         <*  arrowTok
         <*> expr

updateFlag :: Parser UpdateFlag
updateFlag = symbol "u" *> pure Update
         <|> symbol "n" *> pure NoUpdate

expr :: Parser Expr
expr = P.choice [let', case', appF, appC, appP, lit]
  where
    let' = P.try letrecTok <*> binds <* inTok <*> expr
             <|> letTok    <*> binds <* inTok <*> expr

    case' = caseTok <*> expr <* ofTok <*> alts

    appF = AppF <$> varTok <*> atoms

    appC = AppC <$> conTok <*> atoms

    appP = AppP <$> primOp <*> atom <*> atom

    lit = Lit <$> literal

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
literal = Literal . fromInteger <$> P.try L.decimal <* hashTok

primOp :: Parser PrimOp
primOp = P.choice choices <* hashTok
  where
    choices = [ P.char '+' *> pure Add
              , P.char '-' *> pure Sub
              , P.char '*' *> pure Mul
              , P.char '/' *> pure Div
              , P.char '%' *> pure Mod ]

vars :: Parser [Var]
vars = curlied (P.sepBy varTok commaTok)

atoms :: Parser [Atom]
atoms = curlied (P.sepBy atom commaTok)

atom :: Parser Atom
atom = AtomVar <$> varTok
   <|> AtomLit <$> literal
