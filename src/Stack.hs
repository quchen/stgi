{-# LANGUAGE TypeFamilies #-}

module Stack (
    Stack(..),
    popN,
    size,
    (<>>),
) where



import           Data.Foldable as F
import           Data.Monoid


data Stack a = Empty | a :< Stack a

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

(<>>) :: [a] -> Stack a -> Stack a
list <>> stack = fromList list <> stack

-- TODO: This is probably terribly inefficient. Test against _popN and
-- replace if possible.
popN :: Int -> Stack a -> Maybe ([a], Stack a)
popN n stack | n > size stack = Nothing
popN n stack = Just (let (xs, ys) = splitAt n (F.toList stack)
                     in (xs, fromList ys) )

_popN :: Int -> Stack a -> Maybe ([a], Stack a)
_popN n _ | n < 0 = Nothing
_popN 0 stack = Just ([], stack)
_popN _ Empty = Nothing
_popN n (x :< xs) = case popN (n-1) xs of
    Nothing -> Nothing  -- ^ Can't make this "_popN" since that's a hole :-/
    Just (pops, rest) -> Just (x:pops, rest)

fromList :: [a] -> Stack a
fromList = foldr (:<) Empty

size :: Stack a -> Int
size = foldl' (\acc _ -> acc+1) 0
