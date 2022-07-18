{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN.Lambda
-- Copyright   : (C) 2020-2022 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
module Network.AWS.ARN.Lambda
  ( -- * Functions
    Function (..),
    toFunction,
    fromFunction,

    -- ** Function Optics
    _Function,
    fName,
    fQualifier,
  )
where

import Control.Lens (makeLenses)
import Data.Hashable (Hashable)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.AWS.ARN.Internal.Lens (Prism', prism')

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens

-- | An AWS Lambda function name, and optional alias/version qualifier.
--
-- >>> "function:helloworld" ^? _Function
-- Just (Function {_fName = "helloworld", _fQualifier = Nothing})
--
-- >>> "function:helloworld:$LATEST" ^? _Function
-- Just (Function {_fName = "helloworld", _fQualifier = Just "$LATEST"})
--
-- >>> "function:helloworld:42" ^? _Function
-- Just (Function {_fName = "helloworld", _fQualifier = Just "42"})
data Function = Function
  { _fName :: Text,
    _fQualifier :: Maybe Text
  }
  deriving (Eq, Ord, Hashable, Show, Generic)

$(makeLenses ''Function)

toFunction :: Text -> Maybe Function
toFunction t = case T.splitOn ":" t of
  ("function" : name : qual) ->
    Just (Function name) <*> case qual of
      [q] -> Just $ Just q
      [] -> Just Nothing
      _ -> Nothing
  _ -> Nothing

fromFunction :: Function -> Text
fromFunction f =
  T.intercalate ":" $
    ["function", _fName f] ++ maybeToList (_fQualifier f)

_Function :: Prism' Text Function
_Function = prism' fromFunction toFunction
