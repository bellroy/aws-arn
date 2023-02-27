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
        "Object"
        [ testGroup
            "parsing"
            [ testCase "s3 bucket" $
                "bucket-name" ^? _Object @?= Nothing,
              testCase "s3 bucket and object" $
                "bucket-name/my/object" ^? _Object @?= Just (Object "bucket-name" $ "my/object")
            ],
          testGroup
            "updating"
            [ testCase "setting object key" $
                set (_Object . rObjectKey) "my/other/object" "bucket-name/my/object"
                  @?= "bucket-name/my/other/object"
            ]
        ]
    , testGroup
        "S3 bucket"
        [ testGroup
            "parsing"
            [ testCase "s3 bucket" $
                "bucket-name" ^? _Bucket @?= Just (Bucket "bucket-name"),
              testCase "s3 bucket and object" $
                "bucket-name/my/object" ^? _Bucket @?= Nothing
            ]
        ]
    ]

rObjectKey :: Lens' Object Text
rObjectKey l r@Object {objectKey} = (\o -> r {objectKey = o}) <$> l objectKey
