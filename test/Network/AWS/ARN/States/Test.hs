{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.ARN.States.Test where

import Data.Text (Text)
import Lens.Micro.Pro (Lens', set, (^?))
import Network.AWS.ARN.States
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all =
  testGroup
    "Network.AWS.ARN.StateMachine"
    [ testGroup
        "StateMachine"
        [ testGroup
            "parsing"
            [ testCase "state machine name" $
                "stateMachine:workflow" ^? _StateMachine @?= Just (StateMachine "workflow")
            ],
          testGroup
            "updating"
            [ testCase "setting name" $
                set (_StateMachine . smName) "newWorkflow" "stateMachine:workflow"
                  @?= "stateMachine:newWorkflow"
            ]
        ]
    ]

smName :: Lens' StateMachine Text
smName l sm@StateMachine {name} = (\n -> sm {name = n}) <$> l name
