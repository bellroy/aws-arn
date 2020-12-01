{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--
-- Module      : Network.AWS.ARN.Lambda
-- Copyright   : TODO
-- License     : BSD3
-- Maintainer  : TODO
-- Stability   : experimental
module Network.AWS.ARN.Lambda
  ( -- * Functions
    Function(..),
    toFunction,
    fromFunction,
    -- ** Function Optics
    _Function,
    fName,
    fQualifier
  ) where

import Control.Lens
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import GHC.Generics (Generic)

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
      _ -> Nothing
  _ -> Nothing

fromFunction :: Function -> Text
fromFunction f =
  T.intercalate ":" $
    ["function", _fName f] ++ maybeToList (_fQualifier f)

_Function :: Prism' Text Function
_Function = prism' fromFunction toFunction
