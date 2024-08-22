{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN
-- Copyright   : (C) 2020-2022 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- Provides a type representing [Amazon Resource Names
-- (ARNs)](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html),
-- and parsing/unparsing functions for them. The provided optics make it
-- very convenient to rewrite parts of ARNs.
--
-- == Example
--
-- [API Gateway Lambda
-- Authorizers](https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html)
-- are given the ARN of the requested endpoint and method, and are
-- expected to respond with an IAM Policy Document. It is sometimes
-- useful to manipulate the given ARN when describing which resources to
-- authorize.
--
-- Here, we generalize @authorizerSampleARN@ to cover every method of
-- every endpoint in the stage:
--
-- @
-- {-# LANGUAGE OverloadedLabels #-}
-- -- This provides the necessary instances from generic-lens
-- import Data.Generics.Labels ()
--
-- -- Returns "arn:aws:execute-api:us-east-1:123456789012:my-spiffy-api\/stage\/*"
-- let
--   authorizerSampleARN = "arn:aws:execute-api:us-east-1:123456789012:my-spiffy-api\/stage\/GET\/some\/deep\/path"
-- in
--   over ('_ARN' . #resource . 'slashes') (\\parts -> take 2 parts ++ ["*"]) authorizerSampleARN
-- @
module Network.AWS.ARN
  ( ARN (..),
    parseARN,
    renderARN,

    -- * ARN Prism
    _ARN,

    -- * Utility Optics
    colons,
    slashes,
  )
where

import Data.Eq.Deriving (deriveEq1)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
import Data.Ord.Deriving (deriveOrd1)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Generic1)
import Lens.Micro.Pro (Iso', Prism', iso, prism')
import Text.Show.Deriving (deriveShow1)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Function ((&))
-- >>> import Lens.Micro.Pro (from, ix, (.~), (^.), (^?))

-- | A parsed ARN. Either use the '_ARN' 'Prism'', or the 'parseARN' and
-- 'renderARN' functions to convert @'Text' \<-\> 'ARN'@.  The
-- 'resource' part of an ARN will often contain colon- or
-- slash-separated parts which precisely identify some resource. If
-- there is no service-specific module (see below), the 'colons' and
-- 'slashes' optics in this module can pick apart the `resource`
-- field.
--
-- If you want lenses into individual fields, use the
-- [@generic-lens@](https://hackage.haskell.org/package/generic-lens)
-- or
-- [@generic-optics@](https://hackage.haskell.org/package/generic-optics)
-- libraries.
--
-- == Service-Specific Modules
--
-- Modules like "Network.AWS.ARN.Lambda" provide types to parse the
-- resource part of an ARN into something more specific:
--
-- @
-- -- Remark: Lambda._Function :: Prism' Text Lambda.Function
-- -- Returns: Just "the-coolest-function-ever"
-- let
--   functionARN = "arn:aws:lambda:us-east-1:123456789012:function:the-coolest-function-ever:Alias"
-- in
--   functionARN ^? _ARN . #resource . Lambda._Function . #name
-- @
--
-- You can also use 'ARN'\'s 'Traversable' instance and
-- 'Control.Lens.Prism.below' to create 'Prism''s that indicate their
-- resource type in 'ARN'\'s type variable:
--
-- @
-- '_ARN' . 'Control.Lens.Prism.below' Lambda._Function :: Prism' Text ('ARN' Lambda.Function)
-- @
--
-- @since 0.1.0.0
data ARN r = ARN
  { partition :: Text,
    service :: Text,
    region :: Text,
    account :: Text,
    resource :: r
  }
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      Generic1,
      Hashable,
      Functor,
      Foldable,
      Traversable
    )

$(deriveEq1 ''ARN)
$(deriveOrd1 ''ARN)
$(deriveShow1 ''ARN)

deriving instance Hashable1 ARN

-- | @since 0.2.0.0
parseARN :: Text -> Maybe (ARN Text)
parseARN t = case T.splitOn ":" t of
  ("arn" : part : srv : reg : acc : res) ->
    Just $
      ARN
        { partition = part,
          service = srv,
          region = reg,
          account = acc,
          resource = T.intercalate ":" res
        }
  _ -> Nothing

-- | @since 0.2.0.0
renderARN :: ARN Text -> Text
renderARN arn =
  T.intercalate
    ":"
    [ "arn",
      partition arn,
      service arn,
      region arn,
      account arn,
      resource arn
    ]

-- | @since 0.1.0.0
_ARN :: Prism' Text (ARN Text)
_ARN = prism' renderARN parseARN
{-# INLINE _ARN #-}

-- | Split a 'Text' into colon-separated parts.
--
-- This is an improper 'Iso'' (@Text.intercalate ":" . Text.splitOn
-- ":" = id@, but @Text.splitOn ":" . Text.intercalate ":" /= id@).
-- This causes violations of the 'Iso'' laws for lists whose members
-- contain @\':\'@:
--
-- >>> [":"] ^. from colons . colons
-- ["",""]
--
-- The laws are also violated on empty lists:
--
-- >>> [] ^. from colons . colons
-- [""]
--
-- Nevertheless, it is still useful:
--
-- >>> "foo:bar:baz" & colons . ix 1 .~ "quux"
-- "foo:quux:baz"
--
-- Ed [discusses improper
-- optics](https://old.reddit.com/r/haskell/comments/32xva8/the_laws_of_asymmetric_wellbehaved_lenses_are/cqhq1gk/)
-- in an old Reddit comment.
--
-- @since 0.3.0.0
colons :: Iso' Text [Text]
colons = iso (T.splitOn ":") (T.intercalate ":")
{-# INLINE colons #-}

-- | Split a 'Text' into slash-separated parts.
--
-- List 'colons', this is an improper 'Iso'', but it is still useful:
--
-- >>> "foo/bar/baz" ^. slashes
-- ["foo","bar","baz"]
--
-- @since 0.3.0.0
slashes :: Iso' Text [Text]
slashes = iso (T.splitOn "/") (T.intercalate "/")
{-# INLINE slashes #-}
