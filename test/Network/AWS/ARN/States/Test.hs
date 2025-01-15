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
            [ testCase "unqualified state machine name" $
                "stateMachine:workflow" ^? _StateMachine @?= Just (StateMachine "workflow" Nothing),
              testCase "qualified state machine name" $
                "stateMachine:workflow:42" ^? _StateMachine @?= Just (StateMachine "workflow" $ Just "42")
            ],
          testGroup
            "updating"
            [ testCase "setting name" $
                set (_StateMachine . smName) "newWorkflow" "stateMachine:workflow"
                  @?= "stateMachine:newWorkflow",
              testCase "setting qualifier" $
                set (_StateMachine . smQualifier) (Just "v1") "stateMachine:workflow"
                  @?= "stateMachine:workflow:v1",
              testCase "unsetting qualifier" $
                set (_StateMachine . smQualifier) Nothing "stateMachine:workflow:v1"
                  @?= "stateMachine:workflow"
            ]
        ]
    ]

smName :: Lens' StateMachine Text
smName l sm@StateMachine {name} = (\n -> sm {name = n}) <$> l name

smQualifier :: Lens' StateMachine (Maybe Text)
smQualifier l sm@StateMachine {qualifier} = (\q -> sm {qualifier = q}) <$> l qualifier
