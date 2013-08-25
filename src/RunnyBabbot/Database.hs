module RunnyBabbot.Database
    (registerTweet
    ,newTweets)
where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Control.Monad (filterM)

import RunnyBabbot.TwitterData (Tweet(..))

registerTweet :: IConnection conn => conn -> Tweet -> IO ()
registerTweet conn tweet = do
  let Tweet {RunnyBabbot.TwitterData.id = tweet_id} = tweet
  run conn "INSERT INTO tweets (id) VALUES (?)" [SqlInteger tweet_id]
  commit conn
  return ()

newTweets :: IConnection conn => conn -> [Tweet] -> IO ([Tweet])
newTweets conn tweets = filterM (isUnprocessed conn)
                        tweets

isUnprocessed :: IConnection conn => conn -> Tweet -> IO (Bool)
isUnprocessed conn tweet = do
  let Tweet {RunnyBabbot.TwitterData.id = tweet_id} = tweet
  ret <- quickQuery conn "SELECT * FROM tweets WHERE id = ? LIMIT 1"
         [SqlInteger tweet_id]

  return $ length ret == 0

migrate :: IConnection conn => conn -> IO ()
migrate conn = do
  run conn "CREATE TABLE tweets (id INTEGER PRIMARY KEY)" []
  commit conn
  return ()
