{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.ARN.Lambda.Test where

import Data.Text (Text)
import Lens.Micro.Pro (Lens', set, (^?))
import Network.AWS.ARN.Lambda
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all =
  testGroup
    "Network.AWS.ARN.Lambda"
    [ testGroup
        "Function"
        [ testGroup
            "parsing"
            [ testCase "unqualified function name" $
                "function:foo" ^? _Function @?= Just (Function "foo" Nothing),
              testCase "qualified function name" $
                "function:bar:3" ^? _Function @?= Just (Function "bar" $ Just "3")
            ],
          testGroup
            "updating"
            [ testCase "setting qualifier" $
                set (_Function . fQualifier) (Just "baz") "function:foo"
                  @?= "function:foo:baz",
              testCase "unsetting qualifier" $
                set (_Function . fQualifier) Nothing "function:quux:42"
                  @?= "function:quux"
            ]
        ]
    ]

fQualifier :: Lens' Function (Maybe Text)
fQualifier l f@Function {qualifier} = (\q -> f {qualifier = q}) <$> l qualifier
