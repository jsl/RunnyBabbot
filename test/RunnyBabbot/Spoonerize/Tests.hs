module RunnyBabbot.Spoonerize.Tests
    ( tests
    ) where

import qualified Test.HUnit as H
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework                 (Test, testGroup)

import           Text.Printf                    (printf)

import RunnyBabbot.Spoonerize

-- Convenience function stolen from the Hakyll test suite
fromAssertions :: String       -- ^ Name
               -> [H.Assertion]  -- ^ Cases
               -> [Test]       -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]


test1 = H.assertEqual "Returns an annotated sentence"
        [ WordInfo 1 "Hey" True, WordInfo 2 "u" False, WordInfo 3 "There" True]
        (markedSentence "Hey u There")

test2 = H.assertEqual "Does not mark @RunnyBabbot as spoonerizable"
        [WordInfo 1 "@RunnyBabbot" False]
        (markedSentence "@RunnyBabbot")

test3 = H.assertEqual "Does not mark @runnybabbot as spoonerizable"
        [WordInfo 1 "@runnybabbot" False]
        (markedSentence "@runnybabbot")

test4 = H.assertEqual
        "Does not mark @RunnyBabbot as spoonerizable when at end of sentence"
        [WordInfo 1 "@RunnyBabbot." False]
        (markedSentence "@RunnyBabbot.")

test5 = H.assertEqual
        "Does not spoonerize other usernames"
        [WordInfo 1 "Hi" True, WordInfo 2 "@jbrechtel" False]
        (markedSentence "Hi @jbrechtel")

test6 = H.assertEqual
        "Provides all possible spoonerizations"
        ["Bey hunny rabbit", "Rey bunny habbit", "Hey runny babbit"]
        (spoonerizations "Hey bunny rabbit")

tests :: Test
tests = testGroup "RunnyBabbot.Spoonerize.Tests" $
        fromAssertions "annotations" [ test1, test2, test3, test4, test5,
                                       test6 ]
