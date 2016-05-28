{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | A generic parser for parsing indentation-sensitive grammars with Haskell
-- comment syntax.
module Stg.Parser.StgParserType (
    StgParser(..),
    nesting,
) where



import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Text.Parser.LookAhead
import Text.Parser.Token.Highlight
import Text.Trifecta               as Trifecta
import Text.Trifecta.Delta

import Data.Stack



-- | A parser for an STG syntax element.
newtype StgParser ast = StgParser (StateT (Stack Indent) Trifecta.Parser ast)
    deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
             , MonadState (Stack Indent)
             , CharParsing, Parsing, LookAheadParsing, DeltaParsing )

instance TokenParsing StgParser where

    someSpace = do
        void horizontalSpace
        skipOptional (try (do
            void (newline *> many blankLine)
            i <- horizontalSpace
            get >>= \case
                j :< _ -> guard (i > j)
                Empty  -> guard (i > Indent 0) ))

    nesting p = do
        currentIndent <- fmap (mkIndent . column) position
        get >>= \case
            Empty  -> pure ()
            i :< _ -> guard (currentIndent > i)
        pushIndent currentIndent
        result <- p
        popIndent
        someSpace
        pure result

    semi = try (void newline
             *> many blankLine
             *> indent
             *> pure ';' )

-- | An indentation level.
newtype Indent = Indent Int
    deriving (Eq, Ord, Show)

pushIndent :: Indent -> StgParser ()
pushIndent i = modify (i :<)

popIndent :: StgParser ()
popIndent = do
    rest <- get >>= \case
        _ :< is -> pure is
        Empty -> fail "Tried popping an empty indentation stack;\
                      \ please report this as a bug"
    put rest

blankLine :: StgParser ()
blankLine = void (lineRemainder *> newline)

indent :: StgParser ()
indent = try (do
    hSpace <- horizontalSpace
    get >>= \case
        Empty  -> guard (hSpace == Indent 0)
        i :< _ -> guard (hSpace == i) )

lineRemainder :: StgParser ()
lineRemainder = do
    void horizontalSpace
    skipMany (blockComment >> horizontalSpace)
    skipOptional lineComment
    void (lookAhead newline)

blockComment :: TokenParsing parser => parser ()
blockComment = (highlight Comment . void)
    (try (symbol "{-") *> manyTill anyChar (try (symbol "-}")))
    <?> ""

lineComment :: TokenParsing parser => parser ()
lineComment = (highlight Comment . void)
    (try (symbol "--") *> manyTill anyChar newline)
    <?> ""

-- | Consume all horizontal space and return the change in column.
horizontalSpace :: StgParser Indent
horizontalSpace = do
    start <- fmap column position
    void (many (satisfy (\c -> isSpace c && c /= '\n')))
    end <- fmap column position
    pure $! mkIndent (end - start)

mkIndent :: Integral a => a -> Indent
mkIndent = Indent . fromIntegral
