{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | A simple stack type. Very similar to an ordinary list, but with a more
-- specialized API.
module Data.Stack (
    Stack(..),
    forEachPop,
    (<>>),
    span,
) where



import           Data.Foldable                as F
import           Data.Monoid
import qualified GHC.Exts                     as OL
import           Prelude                      hiding (span)
import qualified Prelude                      as P
import           Text.PrettyPrint.ANSI.Leijen hiding (list, (<>))



-- | The usual stack data structure.
data Stack a = Empty | a :< Stack a
    deriving (Eq, Ord)

instance Show a => Show (Stack a) where
    show = show . toList

instance Pretty a => Pretty (Stack a) where
    pretty = prettyList . toList

instance Functor Stack where
    fmap _ Empty = Empty
    fmap f (x :< xs) = f x :< fmap f xs

instance Foldable Stack where
    foldMap _ Empty = mempty
    foldMap f (x :< xs) = f x <> foldMap f xs

instance Monoid (Stack a) where
    mempty = Empty
    Empty `mappend` s = s
    (x :< xs) `mappend` ys = x :< (xs <> ys)

instance OL.IsList (Stack a) where
    type Item (Stack a) = a
    fromList = foldr (:<) Empty
    toList = F.toList

-- | Push a list of items onto the stack. The first item will be at the
-- top of the stack.
(<>>) :: [a] -> Stack a -> Stack a
list <>> stack = foldr (:<) stack list

-- | For each list element, pop one element off the stack. Fail if not enough
-- elements are on the stack.
forEachPop :: [x] -> Stack a -> Maybe ([a], Stack a)
forEachPop (_:_) Empty = Nothing
forEachPop [] stack = Just ([], stack)
forEachPop (_:xs) (s :< stack) = case forEachPop xs stack of
    Nothing -> Nothing
    Just (pops, rest) -> Just (s:pops, rest)

-- | Like 'Prelude.span' for lists.
span :: (a -> Bool) -> Stack a -> (Stack a, Stack a)
span p stack = let (a,b) = P.span p (toList stack)
                in (OL.fromList a, OL.fromList b)
