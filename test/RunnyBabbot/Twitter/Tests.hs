module RunnyBabbot.Twitter.Tests
    ( tests
    ) where

import qualified Test.HUnit as H
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework                 (Test, testGroup)

import RunnyBabbot.Twitter

import qualified Data.Text as T

test1 = H.assertEqual "Replacing the mentioning username"
        "Hey there @AnotherUsername"
        (replaceRunnyBabbotMention "Hey there @RunnyBabbot" "AnotherUsername")

tests :: Test
tests = testGroup "RunnyBabbot.Twitter.Tests"
        [ testCase "replacing mentions" test1 ]
