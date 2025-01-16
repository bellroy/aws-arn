{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.ARNTest where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Lens.Micro.Pro (Lens', over, preview, review)
import Network.AWS.ARN
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all =
  testGroup
    "Network.AWS.ARN"
    [ testGroup
        "basic tests"
        [ testCase "inspect function name and alias of Lambda ARN" $
            (resource <$> parseARN aliasedLambdaSampleARN)
              @?= Just "function:the-coolest-function-ever:Alias",
          testCase "parses empty region OK" $
            (region <$> parseARN s3FileSampleARN) @?= Just "",
          testCase "parses empty account OK" $
            (account <$> parseARN s3FileSampleARN) @?= Just "",
          testCase "rejects non-ARNs" $
            parseARN sampleNotAnARN @?= Nothing
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

arnResource :: Lens' (ARN a) a
arnResource l a@(ARN {resource}) = (\r -> a {resource = r}) <$> l resource

-- In Data.List.NonEmpty as of base >=4.16, but not worth breaking
-- compatibility just for this. Remove once the three latest GHC major
-- releases are all base >=4.16.
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList [] ys = ys
prependList (x : xs) (y :| ys) = x :| xs ++ y : ys
