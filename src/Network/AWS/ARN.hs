{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN
-- Copyright   : (C) 2020-2021 Bellroy Pty Ltd
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
-- -- Returns "arn:aws:execute-api:us-east-1:123456789012:my-spiffy-api\/stage\/*"
-- let
--   authorizerSampleARN = "arn:aws:execute-api:us-east-1:123456789012:my-spiffy-api\/stage\/GET\/some\/deep\/path"
-- in
--   over ('_ARN' . 'arnResource' . 'slashes') (\\parts -> take 2 parts ++ ["*"]) authorizerSampleARN
-- @
module Network.AWS.ARN
  ( ARN (..),
    toARN,
    fromARN,

    -- * ARN Optics
    _ARN,
    arnPartition,
    arnService,
    arnRegion,
    arnAccount,
    arnResource,

    -- * Utility Optics
    colons,
    slashes,
  )
where

import Control.Lens (Iso', Prism', iso, makeLenses, prism')
import Data.Eq.Deriving (deriveEq1)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
import Data.Ord.Deriving (deriveOrd1)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Generic1)
import Text.Show.Deriving (deriveShow1)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens

-- | A parsed ARN. Either use the '_ARN' 'Prism'', or the 'toARN' and
-- 'fromARN' functions to convert @'Text' \<-\> 'ARN'@.  The
-- '_arnResource' part of an ARN will often contain colon- or
-- slash-separated parts which precisely identify some resource. If
-- there is no service-specific module (see below), the 'colons' and
-- 'slashes' @'Control.Lens.Iso''@s in this module can pick apart the
-- `_arnResource` field.
--
-- == Service-Specific Modules
--
-- Modules like "Network.AWS.ARN.Lambda" provide types to parse the
-- resource part of an ARN into something more specific:
--
-- @
-- -- Remark: Lambda._Function :: 'Prism'' 'Text' Lambda.Function
-- -- Returns: Just "the-coolest-function-ever"
-- let
--   functionARN = "arn:aws:lambda:us-east-1:123456789012:function:the-coolest-function-ever:Alias"
-- in
--   functionARN ^? _ARN . arnResource . Lambda._Function . Lambda.fName
-- @
--
-- You can also use 'ARN'\'s 'Traversable' instance and
-- 'Control.Lens.Prism.below' to create 'Prism''s that indicate their
-- resource type in 'ARN'\'s type variable:
--
-- @
-- '_ARN' . 'Control.Lens.Prism.below' Lambda._Function :: 'Prism'' 'Text' ('ARN' Lambda.Function)
-- @
data ARN r = ARN
  { _arnPartition :: Text,
    _arnService :: Text,
    _arnRegion :: Text,
    _arnAccount :: Text,
    _arnResource :: r
  }
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      Generic1,
      Hashable,
      Hashable1,
      Functor,
      Foldable,
      Traversable
    )

$(makeLenses ''ARN)
$(deriveEq1 ''ARN)
$(deriveOrd1 ''ARN)
$(deriveShow1 ''ARN)

toARN :: Text -> Maybe (ARN Text)
toARN t = case T.splitOn ":" t of
  ("arn" : part : srv : reg : acc : res) ->
    Just $
      ARN
        { _arnPartition = part,
          _arnService = srv,
          _arnRegion = reg,
          _arnAccount = acc,
          _arnResource = T.intercalate ":" res
        }
  _ -> Nothing

fromARN :: ARN Text -> Text
fromARN arn =
  T.intercalate
    ":"
    [ "arn",
      _arnPartition arn,
      _arnService arn,
      _arnRegion arn,
      _arnAccount arn,
      _arnResource arn
    ]

_ARN :: Prism' Text (ARN Text)
_ARN = prism' fromARN toARN
{-# INLINE _ARN #-}

-- | Split a 'Text' into colon-separated parts.
--
-- >>> "foo:bar:baz" & colons . ix 1 .~ "quux"
-- "foo:quux:baz"
colons :: Iso' Text [Text]
colons = iso (T.splitOn ":") (T.intercalate ":")
{-# INLINE colons #-}

-- | Split a 'Text' into slash-separated parts.
--
-- >>> "foo/bar/baz" ^. slashes
-- ["foo","bar","baz"]
slashes :: Iso' Text [Text]
slashes = iso (T.splitOn "/") (T.intercalate "/")
{-# INLINE slashes #-}
