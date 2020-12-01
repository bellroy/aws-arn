{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN
-- Copyright   : TODO
-- License     : BSD3
-- Maintainer  : TODO
-- Stability   : experimental
--
-- Provides a type representing [Amazon Resource Names
-- (ARNs)](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html),
-- and parsing/unparsing functions for them. The provided optics make it
-- very convenient to rewrite parts of ARNs.
--
-- == Utility Optics
--
-- The resource identifier part of an ARN often contains colon- or
-- slash-separated parts which precisely identify a resource. In such
-- situations, the 'colons' and 'slashes' @Iso@s provided here are
-- helpful.
--
-- == Service-Specific ARNs
--
-- Modules like "Network.AWS.ARN.Lambda" provide types to parse the
-- resource part of an ARN into something more specific:
--
-- @
-- -- Returns: Just "the-coolest-function-ever"
-- let
--   functionARN = "arn:aws:lambda:us-east-1:123456789012:function:the-coolest-function-ever:Alias"
-- in
--   functionARN ^? _ARN . arnResource . Lambda._Function . Lambda.fName
-- @
--
-- 'ARN' has a 'Traversable' instance, so it's also possible to
-- assemble prisms for these more specific ARN types. Use
-- 'Control.Lens.Prism.below':
--
-- @
-- '_ARN' . 'Control.Lens.Prism.below' Lambda._Function :: 'Prism'' 'Text' ('ARN' Lambda.Function)
-- @
--
-- PRs to add parsers for more resource types are **especially** welcome.
--
-- == Examples
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
--   over ('_ARN' . 'resource' . 'slashes') (\parts -> take 2 parts ++ ["*"]) authorizerSampleARN
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
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data ARN r = ARN
  { _arnPartition :: Text,
    _arnService :: Text,
    _arnRegion :: Text,
    _arnAccount :: Text,
    _arnResource :: r
  }
  deriving (Eq, Ord, Show, Generic, Hashable, Functor, Foldable, Traversable)

$(makeLenses ''ARN)

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
-- ["foo", "bar", "baz"]
slashes :: Iso' Text [Text]
slashes = iso (T.splitOn "/") (T.intercalate "/")
{-# INLINE slashes #-}
