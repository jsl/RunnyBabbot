module Main where

import RunnyBabbot.Spoonerize (spoonerize, spoonerizations)
import RunnyBabbot.Configuration
import RunnyBabbot.Twitter ( mentions
                           , postTweetResponse
                           , tweetResponseFor
                           , processNewTweets
                           )

import RunnyBabbot.Database (registerTweet, newTweets)

import System.IO (stdout, stderr, hPutStrLn)
import System.Environment (getArgs)
import Control.Monad.Trans (liftIO)
import Data.Text (pack, unpack)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

import System.Console.GetOpt
import Data.Maybe ( fromMaybe )

data Options = Options
    { optConfigFilepath :: FilePath
    , optDbFilepath     :: FilePath
    } deriving Show

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['d'] ["dbfile"] (ReqArg (\ d opts ->
                                   opts { optDbFilepath = d }) "dbfile")
                 "dbfile FILE"
    , Option ['c']  ["configfile"] (ReqArg (\ d opts ->
                                        opts { optConfigFilepath = d })
                                    "configfile")
                 "configfile FILE"
    ]

defaultOptions    = Options
                    { optDbFilepath      = "tweets.db"
                    , optConfigFilepath  = "twitter.cfg"
                    }

botOpts :: [String] -> IO (Options, [String])
botOpts argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip Prelude.id) defaultOptions o, n)
      (_,_,errs) -> ioError
                    (userError (concat errs ++ usageInfo header options))
    where header = "Usage: runny-babbot [OPTION...] .."

main :: IO ()
main = do
  args <- getArgs
  (Options { optDbFilepath = dbfile
           , optConfigFilepath = configfile }, args) <- botOpts args

  dbconn <- connectSqlite3 dbfile
  cfg <- liftIO $ credentials configfile

  case cfg of
    Left err    -> hPutStrLn stderr $ "Incorrect Twitter config: " ++ show err
    Right creds -> do
        ets <- mentions creds
        case ets of
          Left err -> hPutStrLn stderr $ "Unable to retrieve tweets: " ++ err
          Right ts -> do
                     unrespondedTweets <- newTweets dbconn ts
                     putStrLn $ "We are going to respond to: " ++
                              show unrespondedTweets
                     processNewTweets dbconn creds unrespondedTweets
