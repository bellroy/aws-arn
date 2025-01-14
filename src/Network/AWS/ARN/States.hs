{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.ARN.States
  ( StateMachine (..),
    parseStateMachine,
    _StateMachine,
  )
where

import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro.Pro (Prism', prism')

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Lens.Micro.Pro ((^?))

-- | An AWS State Machine, made of its name.
--
-- >>> "stateMachine:orderProcessor" ^? _StateMachine
-- Just (StateMachine {name = "orderProcessor"})
--
-- @since 0.3.3.0
newtype StateMachine = StateMachine
  { name :: Text
  }
  deriving (Eq, Ord, Hashable, Show, Generic)

-- | @since 0.3.3.0
parseStateMachine :: Text -> Maybe StateMachine
parseStateMachine t = case T.splitOn ":" t of
  ["stateMachine", nam] ->
    Just (StateMachine nam)
  _ -> Nothing

-- | @since 0.3.3.0
renderStateMachine :: StateMachine -> Text
renderStateMachine (StateMachine nam) = "stateMachine:" <> nam

-- | @since 0.3.3.0
_StateMachine :: Prism' Text StateMachine
_StateMachine = prism' renderStateMachine parseStateMachine
