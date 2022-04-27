{-# LANGUAGE KindSignatures, TypeFamilies, FlexibleContexts, RankNTypes, TypeFamilyDependencies, LambdaCase #-}


module Traversals where

import Data.Bifunctor


type family Pattern (f :: * -> *) = (p :: * -> * -> *) | p -> f

type family Base t where
    Base (t a) = Pattern t a


class Functor (Base t) => Recursive t where
    project :: t -> Base t t

class Functor (Base t) => Corecursive t where
    embed :: Base t t -> t

class Sequenced f where
    chain :: Applicative g => Pattern f (g a) (g b) -> g (Pattern f a b)


data ListF a b = Nil | Cons a b

type instance Pattern [] = ListF

instance Functor (ListF a) where
    fmap _ Nil        = Nil
    fmap f (Cons a b) = Cons a (f b)

instance Bifunctor ListF where
    bimap _ _ Nil        = Nil
    bimap f g (Cons a b) = Cons (f a) (g b)

instance Recursive [a] where
    project []     = Nil
    project (x:xs) = Cons x xs

instance Corecursive [a] where
    embed Nil        = []
    embed (Cons a b) = a:b

instance Sequenced [] where
    chain Nil        = pure Nil
    chain (Cons a b) = Cons <$> a <*> b


cata :: Recursive (f a) => (Base (f a) b -> b) -> f a -> b
cata f x = f (cata f <$> project x)

trav :: (Sequenced t, Bifunctor (Pattern t), Corecursive (t b), Recursive (t a), Applicative f) => (a -> f b) -> t a -> f (t b)
trav f x = embed <$> chain (bimap f (trav f) $ project x)

trav' :: (Recursive (t a), Corecursive (t b), Functor f) => (Pattern t a (f (t b)) -> f (Pattern t b (t b))) -> t a -> f (t b)
trav' f x = fmap embed (f $ fmap (trav' f) (project x))


printList :: Show a => [a] -> IO [a]
printList = trav (\x -> print x *> pure x)

printList' :: Show a => [a] -> IO [a]
printList' = trav' (\case
        Nil      -> pure Nil
        Cons a b -> Cons <$> (print a *> pure a) <*> b
    )