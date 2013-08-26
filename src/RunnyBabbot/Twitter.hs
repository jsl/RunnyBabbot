{-# LANGUAGE OverloadedStrings #-}

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

import Database.HDBC (IConnection)
import Network.HTTP.Conduit (withManager, httpLbs, parseUrl, urlEncodedBody,
                             responseBody)
import Web.Authenticate.OAuth ( OAuth, Credential, newCredential, newOAuth
                              , signOAuth, oauthConsumerKey, oauthConsumerSecret
                              , oauthServerName)
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

  req <- parseUrl "https://api.twitter.com/1.1/statuses/mentions_timeline.json"

  res <- withManager $ \m -> do
           signedreq <- signOAuth oauth cred req
           httpLbs signedreq m

  return $ eitherDecode $ responseBody res

processTweetResponse :: IConnection conn => conn -> OauthCredentials -> Tweet
                     -> IO ()
processTweetResponse conn creds newTweet = do
  tweetResponse <- tweetResponseFor newTweet
  putStrLn $ "Responding to Tweet: " ++ show newTweet ++
           " with response: " ++ show tweetResponse

  -- Register Tweet in the database first so that we don't spam this person
  -- if the Tweet posting fails. For example, the Tweet post could be too
  -- long. We don't want to continue trying in that case.
  registerTweet conn newTweet
  postTweetResponse creds tweetResponse


processNewTweets :: IConnection conn => conn -> OauthCredentials -> [Tweet] ->
                    IO ()
processNewTweets conn creds = mapM_ (processTweetResponse conn creds)

tweetResponseFor :: Tweet -> IO TweetResponse
tweetResponseFor Tweet { text = txt
                       , RunnyBabbot.TwitterData.id = originalTweetId
                       , user = User { screen_name = userName } } = do

  spoonerizedText <- spoonerize $ unpack txt
  let textWithUserMention = replaceRunnyBabbotMention spoonerizedText
                            (unpack userName)

  return TweetResponse { status = textWithUserMention
                       , in_reply_to_status_id = originalTweetId }

replaceRunnyBabbotMention :: String -> String -> String
replaceRunnyBabbotMention originalTweet newUsername =
    replace "@RunnyBabbot" ('@' : newUsername) originalTweet

postTweetResponse :: OauthCredentials -> TweetResponse -> IO ()
postTweetResponse creds spoonerizedTweetResponse = do
  let TweetResponse { status = st
                    , in_reply_to_status_id = inReplyTo } =
                                                   spoonerizedTweetResponse

  req' <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let req = urlEncodedBody [
             ("status",                (TE.encodeUtf8 . pack) st),
             ("in_reply_to_status_id", (TE.encodeUtf8 . pack . show) inReplyTo)]
            req'

  let oauth = myoauth creds
  let cred  = mycred creds

  putStrLn $ "Posting tweet response: " ++ show req

  res <- withManager $ \m -> do
    signedreq <- signOAuth oauth cred req
    httpLbs signedreq m

  return ()
