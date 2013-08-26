{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module RunnyBabbot.Twitter
    ( mentions
    , postTweetResponse
    , tweetResponseFor
    , replaceRunnyBabbotMention
    , processNewTweets
    ) where

import RunnyBabbot.Spoonerize (spoonerize)
import RunnyBabbot.Configuration (OauthCredentials(..), credentials)
import RunnyBabbot.Database (registerTweet)
import RunnyBabbot.TwitterData (User(..), Tweet(..), TweetResponse(..))

import Database.HDBC
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Text (Text, pack, unpack)
import Data.Aeson (eitherDecode)
import Data.List.Utils (replace)
import qualified Data.Text.Encoding as TE
import Control.Monad (mapM_)

myoauth :: OauthCredentials -> OAuth
myoauth (OauthCredentials
         {consumerKey = ckey, consumerSecret = csecret}) =
    newOAuth { oauthServerName     = "api.twitter.com"
             , oauthConsumerKey    = ckey
             , oauthConsumerSecret = csecret }

mycred :: OauthCredentials -> Credential
mycred (OauthCredentials { accessToken = atoken,
                           accessTokenSecret = atokenSecret}) =
  newCredential atoken atokenSecret

mentions :: OauthCredentials -> IO (Either String [Tweet])
mentions creds = do
  let oauth = myoauth creds
  let cred  = mycred creds

  req <-
      parseUrl $ "https://api.twitter.com/1.1/statuses/mentions_timeline.json"

  res <- withManager $ \m -> do
           signedreq <- signOAuth oauth cred req
           httpLbs signedreq m

  return $ eitherDecode $ responseBody res

processNewTweets :: IConnection conn => conn -> OauthCredentials -> [Tweet] ->
                    IO ()
processNewTweets conn creds newTweets = do
  putStrLn $ "Processing: " ++ show newTweets
  mapM_ (registerTweet conn) newTweets
  mapM_ (\tweet -> ((postTweetResponse creds) .
                    tweetResponseFor) tweet) newTweets

tweetResponseFor :: Tweet -> IO (TweetResponse)
tweetResponseFor Tweet { text = txt
                       , RunnyBabbot.TwitterData.id   = orig_id
                       , user = User { name = user_name } } = do

  spoonerizedText <- spoonerize $ unpack txt
  let textWithUserMention = replaceRunnyBabbotMention spoonerizedText
                            (unpack user_name)

  return $ TweetResponse { status = textWithUserMention
                         , in_reply_to_status_id = orig_id }

replaceRunnyBabbotMention :: String -> String -> String
replaceRunnyBabbotMention originalTweet newUsername =
    replace "@RunnyBabbot" ("@" ++ newUsername) originalTweet

postTweetResponse :: OauthCredentials -> IO TweetResponse -> IO ()
postTweetResponse creds spoonerizedTweetResponse = do
  TweetResponse { status = st
                , in_reply_to_status_id = inReplyTo } <-
                                                   spoonerizedTweetResponse

  req' <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let req = urlEncodedBody [
             ("status",                (TE.encodeUtf8 . pack) st),
             ("in_reply_to_status_id", (TE.encodeUtf8 . pack . show) inReplyTo)]
            req'

  let oauth = myoauth creds
  let cred  = mycred creds

  res <- withManager $ \m -> do
    signedreq <- signOAuth oauth cred req
    httpLbs signedreq m

  return ()
