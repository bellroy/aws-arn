{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.ARN.S3.Test where

import Data.Text (Text)
import Network.AWS.ARN.Internal.Lens (Lens', set, (^?))
import Network.AWS.ARN.S3
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all =
  testGroup
    "Network.AWS.ARN.S3"
    [ testGroup
        "S3Resource"
        [ testGroup
            "parsing"
            [ testCase "s3 bucket" $
                "bucket-name" ^? _S3Resource @?= Just (S3Resource "bucket-name" Nothing),
              testCase "s3 bucket and object" $
                "bucket-name/my/object" ^? _S3Resource @?= Just (S3Resource "bucket-name" $ Just "/my/object")
            ],
          testGroup
            "updating"
            [ testCase "setting object key" $
                set (_S3Resource . rObjectKey) (Just "/my/other/object") "bucket-name/my/object"
                  @?= "bucket-name/my/other/object",
              testCase "unsetting object key" $
                set (_S3Resource . rObjectKey) Nothing "bucket-name/my/object"
                  @?= "bucket-name"
            ]
        ]
    ]

rObjectKey :: Lens' S3Resource (Maybe Text)
rObjectKey l r@S3Resource {objectKey} = (\o -> r {objectKey = o}) <$> l objectKey
