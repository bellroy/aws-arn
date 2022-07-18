{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
  )
where

import Data.Hashable (Hashable)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.AWS.ARN.Internal.Lens (Prism', prism')

-- | An AWS Lambda function name, and optional alias/version qualifier.
--
-- >>> "function:helloworld" ^? _Function
-- Just (Function {name = "helloworld", qualifier = Nothing})
--
-- >>> "function:helloworld:$LATEST" ^? _Function
-- Just (Function {name = "helloworld", qualifier = Just "$LATEST"})
--
-- >>> "function:helloworld:42" ^? _Function
-- Just (Function {name = "helloworld", qualifier = Just "42"})
data Function = Function
  { name :: Text,
    qualifier :: Maybe Text
  }
  deriving (Eq, Ord, Hashable, Show, Generic)

toFunction :: Text -> Maybe Function
toFunction t = case T.splitOn ":" t of
  ("function" : nam : qual) ->
    Just (Function nam) <*> case qual of
      [q] -> Just $ Just q
      [] -> Just Nothing
      _ -> Nothing
  _ -> Nothing

fromFunction :: Function -> Text
fromFunction f =
  T.intercalate ":" $
    ["function", name f] ++ maybeToList (qualifier f)

_Function :: Prism' Text Function
_Function = prism' fromFunction toFunction
