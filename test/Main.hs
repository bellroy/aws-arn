{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens (over, preview, review)
import Data.Text (Text)
import Network.AWS.ARN
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ testGroup
        "basic tests"
        [ testCase "inspect function name and alias of Lambda ARN" $
            (_arnResource <$> toARN aliasedLambdaSampleARN)
              @?= Just "function:the-coolest-function-ever:Alias",
          testCase "parses empty region OK" $
            (_arnRegion <$> toARN s3FileSampleARN) @?= Just "",
          testCase "parses empty account OK" $
            (_arnAccount <$> toARN s3FileSampleARN) @?= Just "",
          testCase "rejects non-ARNs" $
            toARN sampleNotAnARN @?= Nothing
        ],
      testGroup
        "optic tests"
        [ testCase "prism roundtrip" $
            (review _ARN <$> preview _ARN authorizerSampleARN)
              @?= Just authorizerSampleARN,
          testCase "edit path of Lambda Authorizer ARN" $
            over (_ARN . arnResource . slashes) (\parts -> take 2 parts ++ ["*"]) authorizerSampleARN
              @?= "arn:aws:execute-api:us-east-1:123456789012:my-spiffy-api/stage/*"
        ]
    ]

authorizerSampleARN :: Text
authorizerSampleARN = "arn:aws:execute-api:us-east-1:123456789012:my-spiffy-api/stage/GET/some/deep/path"

aliasedLambdaSampleARN :: Text
aliasedLambdaSampleARN = "arn:aws:lambda:us-east-1:123456789012:function:the-coolest-function-ever:Alias"

s3FileSampleARN :: Text
s3FileSampleARN = "arn:aws:s3:::an-okay-bucket/sample.txt"

sampleNotAnARN :: Text
sampleNotAnARN = "//library.googleapis.com/shelves/shelf1/books/book2"
