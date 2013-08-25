module Main
    ( main
    ) where

import  Test.Framework (defaultMain)

import qualified RunnyBabbot.Twitter.Tests
import qualified RunnyBabbot.Spoonerize.Tests

main :: IO ()
main = defaultMain
    [ RunnyBabbot.Twitter.Tests.tests
    , RunnyBabbot.Spoonerize.Tests.tests ]
