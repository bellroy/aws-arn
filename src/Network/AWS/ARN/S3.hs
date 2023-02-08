{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN.S3
-- Copyright   : (C) 2020-2022 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
module Network.AWS.ARN.S3
  ( -- * S3 Resource
    S3Resource (..),
    parseS3Resource,
    renderS3Resource,

    -- ** Prisms
    _S3Resource,
  )
where

import Data.Foldable (fold)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.AWS.ARN.Internal.Lens (Prism', prism')

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Network.AWS.ARN.Internal.Lens ((^?))

-- | An AWS S3 bucket, and optional object key.
--
-- >>> "bucket-name" ^? _S3Resource
-- Just (S3Resource {bucketName = "bucket-name", objectKey = Nothing})
--
-- >>> "bucket-name/my/object" ^? _S3Resource
-- Just (S3Resource {bucketName = "bucket-name", objectKey = Just "/my/object"})
data S3Resource = S3Resource
  { bucketName :: Text,
    objectKey :: Maybe Text
  }
  deriving (Eq, Ord, Hashable, Show, Generic)

parseS3Resource :: Text -> Maybe S3Resource
parseS3Resource t = case T.breakOn "/" t of
  ("", _) -> Nothing
  (bucket, "") -> Just $ S3Resource bucket Nothing
  (bucket, object) -> Just $ S3Resource bucket (Just object)

renderS3Resource :: S3Resource -> Text
renderS3Resource r =
  bucketName r <> fold (objectKey r)

_S3Resource :: Prism' Text S3Resource
_S3Resource = prism' renderS3Resource parseS3Resource
