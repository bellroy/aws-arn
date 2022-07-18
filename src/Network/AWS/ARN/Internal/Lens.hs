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
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (Choice (..))
import Data.Tagged (Tagged (..))

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

type Setter s a = (a -> Identity a) -> s -> Identity s

set :: Setter s a -> a -> s -> s
set l = over l . const
{-# INLINE set #-}

over :: Setter s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

type Prism' s a =
  forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)

prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' to from p = dimap from' to' $ right' p
  where
    to' :: Applicative f => Either s (f a) -> f s
    to' = either pure (fmap to)

    from' :: s -> Either s a
    from' s = maybe (Left s) Right $ from s
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
