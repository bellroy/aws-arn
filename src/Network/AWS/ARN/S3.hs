{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN.S3
-- Copyright   : (C) 2020-2023 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
module Network.AWS.ARN.S3
  ( -- * S3 Object
    Object (..),
    parseObject,
    renderObject,

    -- ** Prisms
    _Object,

    -- ** Conversions to bucket names
    objectInBucket,
    containingBucket,

    -- * S3 Bucket
    Bucket (..),
    parseBucket,
    renderBucket,

    -- ** Prisms
    _Bucket,
  )
where

import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.AWS.ARN.Internal.Lens (Prism', prism')

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Network.AWS.ARN.Internal.Lens ((^?))

-- | An AWS S3 object, made of a bucket and an object key.
--
-- >>> "bucket-name/my/object" ^? _Object
-- Just (Object {bucket = Bucket {bucketName = "bucket-name"}, objectKey = "my/object"})

-- >>> "bucket-name" ^? _Object
-- Nothing

-- >>> containingBucket $ "bucket-name/my/object" ^? _Object
-- Just (Bucket {bucketName = "bucket-name"})
-- @since 0.3.1.0
data Object = Object
  { bucket :: Bucket,
    objectKey :: Text
  }
  deriving (Eq, Ord, Hashable, Show, Generic)

-- @since 0.3.1.0
objectInBucket :: Text -> Bucket -> Object
objectInBucket objectKey bucket = Object bucket objectKey

-- @since 0.3.1.0
containingBucket :: Object -> Bucket
containingBucket = bucket

-- @since 0.3.1.0
parseObject :: Text -> Maybe Object
parseObject t = case T.breakOn "/" t of
  ("", _) -> Nothing
  (_, "") -> Nothing
  (bucketName, object) -> Just $ Object (Bucket bucketName) (T.drop 1 object)

-- @since 0.3.1.0
renderObject :: Object -> Text
renderObject Object {bucket, objectKey} =
  renderBucket bucket <> "/" <> objectKey

-- @since 0.3.1.0
_Object :: Prism' Text Object
_Object = prism' renderObject parseObject

-- | An AWS S3 bucket, without an object key.
--
-- >>> "bucket-name" ^? _Bucket
-- Just (Bucket {bucketName = "bucket-name"})

-- >>> "bucket-name/my/object" ^? _Bucket
-- Nothing
-- @since 0.3.1.0
newtype Bucket = Bucket {bucketName :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- @since 0.3.1.0
parseBucket :: Text -> Maybe Bucket
parseBucket t = case T.breakOn "/" t of
  (bucket, "") -> Just $ Bucket bucket
  _ -> Nothing

-- @since 0.3.1.0
renderBucket :: Bucket -> Text
renderBucket Bucket {bucketName} = bucketName

-- @since 0.3.1.0
_Bucket :: Prism' Text Bucket
_Bucket = prism' renderBucket parseBucket
