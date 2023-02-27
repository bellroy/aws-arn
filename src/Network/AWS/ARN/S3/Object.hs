{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN.S3.Object
-- Copyright   : (C) 2020-2023 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
module Network.AWS.ARN.S3.Object
  ( -- * S3 Object
    S3Object (..),
    parseS3Object,
    renderS3Object,

    -- ** Prisms
    _S3Object,

    -- ** Conversions to bucket names
    objectInBucket,
    containingBucket,
  )
where

import Data.Foldable (fold)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.AWS.ARN.Internal.Lens (Prism', prism')
import qualified Network.AWS.ARN.S3.Bucket as Bucket

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Network.AWS.ARN.Internal.Lens ((^?))

-- | An AWS S3 object, made of a bucket and an object key.
--
-- >>> "bucket-name/my/object" ^? _S3Object
-- Just (S3Object {bucketName = "bucket-name", objectKey = "my/object"})

-- >>> "bucket-name" ^? _S3Object
-- Nothing

-- >>> containingBucket $ "bucket-name/my/object" ^? _S3Object
-- Just (S3Bucket {bucketName = "bucket-name"})
data S3Object = S3Object
  { bucketName :: Text,
    objectKey :: Text
  }
  deriving (Eq, Ord, Hashable, Show, Generic)

objectInBucket :: Text -> Bucket.S3Bucket -> S3Object
objectInBucket objectKey (Bucket.S3Bucket bucketName) = S3Object bucketName objectKey

containingBucket :: S3Object -> Bucket.S3Bucket
containingBucket = Bucket.S3Bucket . bucketName

parseS3Object :: Text -> Maybe S3Object
parseS3Object t = case T.breakOn "/" t of
  ("", _) -> Nothing
  (_, "") -> Nothing
  (bucket, object) -> Just $ S3Object bucket (T.drop 1 object)

renderS3Object :: S3Object -> Text
renderS3Object r =
  bucketName r <> "/" <> objectKey r

_S3Object :: Prism' Text S3Object
_S3Object = prism' renderS3Object parseS3Object
