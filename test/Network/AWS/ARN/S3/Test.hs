{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.ARN.S3.Test where

import Data.Text (Text)
import Network.AWS.ARN.Internal.Lens (Lens', set, (^?))
import Network.AWS.ARN.S3.Bucket
import Network.AWS.ARN.S3.Object
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all =
  testGroup
    "Network.AWS.ARN.S3"
    [ testGroup
        "S3Object"
        [ testGroup
            "parsing"
            [ testCase "s3 bucket" $
                "bucket-name" ^? _S3Object @?= Nothing,
              testCase "s3 bucket and object" $
                "bucket-name/my/object" ^? _S3Object @?= Just (S3Object "bucket-name" $ "my/object")
            ],
          testGroup
            "updating"
            [ testCase "setting object key" $
                set (_S3Object . rObjectKey) "my/other/object" "bucket-name/my/object"
                  @?= "bucket-name/my/other/object"
            ]
        ]
    , testGroup
        "S3 bucket"
        [ testGroup
            "parsing"
            [ testCase "s3 bucket" $
                "bucket-name" ^? _S3Bucket @?= Just (S3Bucket "bucket-name"),
              testCase "s3 bucket and object" $
                "bucket-name/my/object" ^? _S3Bucket @?= Nothing
            ]
        ]
    ]

rObjectKey :: Lens' S3Object Text
rObjectKey l r@S3Object {objectKey} = (\o -> r {objectKey = o}) <$> l objectKey
