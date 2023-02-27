{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN.S3.Bucket
-- Copyright   : (C) 2020-2023 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
module Network.AWS.ARN.S3.Bucket
  ( -- * S3 Bucket
    S3Bucket (..),
    parseS3Bucket,
    renderS3Bucket,

    -- ** Prisms
    _S3Bucket,
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

-- | An AWS S3 bucket, without an object key.
--
-- >>> "bucket-name" ^? _S3Bucket
-- Just (S3Bucket {bucketName = "bucket-name"})

-- >>> "bucket-name/my/object" ^? _S3Bucket
-- Nothing
-- @since 0.3.1.0
newtype S3Bucket = S3Bucket
  { bucketName :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- @since 0.3.1.0
parseS3Bucket :: Text -> Maybe S3Bucket
parseS3Bucket t = case T.breakOn "/" t of
  (bucket, "") -> Just $ S3Bucket bucket
  _ -> Nothing

-- @since 0.3.1.0
renderS3Bucket :: S3Bucket -> Text
renderS3Bucket = bucketName

-- @since 0.3.1.0
_S3Bucket :: Prism' Text S3Bucket
_S3Bucket = prism' renderS3Bucket parseS3Bucket
