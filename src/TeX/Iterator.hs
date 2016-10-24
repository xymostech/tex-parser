{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs  #-}

module TeX.Iterator
( Iterator, next
, PeekableIterator, peekable, peek
, dropWhileIt
)
where

class Iterator as a | as -> a where
    next :: as -> (Maybe a, as)

instance Iterator [a] a where
    next [] = (Nothing, [])
    next as = (Just $ head as, tail as)

data PeekableIterator ps p where
    PeekableIterator :: (Iterator as p) => as -> Maybe p -> PeekableIterator as p

peekable :: Iterator as p => as -> PeekableIterator as p
peekable it = PeekableIterator it Nothing

instance (Show ps, Show p) => Show (PeekableIterator ps p) where
    show (PeekableIterator ps p) = "PeekableIterator " ++ show ps ++ " " ++ show p

peek :: Iterator as p => PeekableIterator as p -> (Maybe p, PeekableIterator as p)
peek (PeekableIterator as Nothing) = (a, PeekableIterator as' a)
    where
      (a, as') = next as
peek (PeekableIterator as a) = (a, PeekableIterator as a)

instance Iterator as p => Iterator (PeekableIterator as p) p where
    next (PeekableIterator as Nothing) = (a, PeekableIterator as' Nothing)
        where
          (a, as') = next as
    next (PeekableIterator as a) = (a, PeekableIterator as Nothing)

dropWhileIt :: Iterator as a => as -> (a -> Bool) -> as
dropWhileIt it f =
    case x of
      (Just x')
          | f x'      -> dropWhileIt it' f
          | otherwise -> it
      Nothing -> it'
    where
      (x, it') = next it
