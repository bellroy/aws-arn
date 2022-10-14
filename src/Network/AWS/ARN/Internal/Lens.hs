{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--
-- Module      : Network.AWS.ARN.Internal.Lens
-- Copyright   : (C) 2020-2022 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- Reimplement a few lens types and combinators to keep the dependency
-- footprint down.
module Network.AWS.ARN.Internal.Lens where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (First (..))
import Data.Profunctor (Profunctor (..))
import Data.Profunctor.Choice (Choice (..))
import Data.Tagged (Tagged (..))

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Setter s a = (a -> Identity a) -> s -> Identity s

set :: Setter s a -> a -> s -> s
set l = over l . const
{-# INLINE set #-}

(.~) :: Setter s a -> a -> s -> s
(.~) = set

infixr 4 .~

over :: Setter s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

(^.) :: s -> Getting a s a -> a
s ^. l = getConst $ l Const s

infixl 8 ^.

type Prism' s a =
  forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)

prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' inj prj p = dimap prj' inj' $ right' p
  where
    inj' :: Applicative f => Either s (f a) -> f s
    inj' = either pure (fmap inj)

    prj' :: s -> Either s a
    prj' s = maybe (Left s) Right $ prj s
{-# INLINE prism' #-}

preview :: Prism' s a -> s -> Maybe a
preview p s = (getFirst . getConst . ($ s)) $ p (Const . First . Just)
{-# INLINE preview #-}

review :: Prism' s a -> a -> s
review p = runIdentity . unTagged . p . Tagged . Identity
{-# INLINE review #-}

(^?) :: s -> Prism' s a -> Maybe a
s ^? p = preview p s
{-# INLINE (^?) #-}

infixl 8 ^?

type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

ix :: Int -> Traversal' [a] a
ix 0 f (x : xs) = (: xs) <$> f x
ix n f (x : xs) = (x :) <$> ix (n - 1) f xs
ix _ _ [] = pure []

type Iso' s a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

type AnIso' s a = Exchange a a a (Identity a) -> Exchange a a s (Identity s)

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)

iso :: (s -> a) -> (a -> s) -> Iso' s a
iso f t = dimap f (fmap t)
{-# INLINE iso #-}

from :: AnIso' s a -> Iso' a s
from l = iso (runIdentity . t) f
  where
    Exchange f t = l $ Exchange id Identity
