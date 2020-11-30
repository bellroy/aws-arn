{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
--   over ('_ARN' . 'resource' . 'slashes') (\parts -> take 2 parts ++ ["*"]) authorizerSampleARN
-- @
module Network.AWS.ARN
  ( ARN (..),
    fromText,
    toText,

    -- * ARN Optics
    _ARN,
    partition,
    service,
    region,
    account,
    resource,

    -- * Utility Optics
    colons,
    slashes,
  )
where

import Data.Profunctor (Choice (..), Profunctor (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data ARN = ARN
  { _partition :: Text,
    _service :: Text,
    _region :: Text,
    _account :: Text,
    _resource :: Text
  }
  deriving (Eq, Show, Generic)

fromText :: Text -> Maybe ARN
fromText t = case T.splitOn ":" t of
  ("arn" : part : srv : reg : acc : res) ->
    Just $
      ARN
        { _partition = part,
          _service = srv,
          _region = reg,
          _account = acc,
          _resource = T.intercalate ":" res
        }
  _ -> Nothing

toText :: ARN -> Text
toText arn =
  T.intercalate
    ":"
    [ "arn",
      _partition arn,
      _service arn,
      _region arn,
      _account arn,
      _resource arn
    ]

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

type Prism' s a =
  forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)

type Iso' s a =
  forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

{-# INLINE _ARN #-}
_ARN :: Prism' Text ARN
_ARN =
  dimap
    (\t -> maybe (Left t) Right $ fromText t)
    (either pure (fmap toText))
    . right'

{-# INLINE partition #-}
partition :: Lens' ARN Text
partition f arn = (\t -> arn {_partition = t}) <$> f (_partition arn)

{-# INLINE service #-}
service :: Lens' ARN Text
service f arn = (\t -> arn {_service = t}) <$> f (_service arn)

{-# INLINE region #-}
region :: Lens' ARN Text
region f arn = (\t -> arn {_region = t}) <$> f (_region arn)

{-# INLINE account #-}
account :: Lens' ARN Text
account f arn = (\t -> arn {_account = t}) <$> f (_account arn)

{-# INLINE resource #-}
resource :: Lens' ARN Text
resource f arn = (\t -> arn {_resource = t}) <$> f (_resource arn)

{-# INLINE colons #-}

-- | Split a 'Text' into colon-separated parts.
--
-- >>> "foo:bar:baz" & colons . ix 1 .~ "quux"
-- "foo:quux:baz"
colons :: Iso' Text [Text]
colons = dimap (T.splitOn ":") (fmap (T.intercalate ":"))

{-# INLINE slashes #-}

-- | Split a 'Text' into slash-separated parts.
--
-- >>> "foo/bar/baz" ^. slashes
-- ["foo", "bar", "baz"]
slashes :: Iso' Text [Text]
slashes = dimap (T.splitOn "/") (fmap (T.intercalate "/"))
