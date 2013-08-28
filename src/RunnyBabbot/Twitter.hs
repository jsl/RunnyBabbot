{-# LANGUAGE OverloadedStrings #-}

module RunnyBabbot.Twitter
    ( mentions
    , postTweetResponse
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
processTweetResponse conn creds
                     Tweet { text = txt
                           , RunnyBabbot.TwitterData.id = originalTweetId
                           , user = User { screen_name = userName } } = do

                       -- Register before spoonerizing, since we don't want
                       -- to reprocess old Tweets
                       putStrLn $ "Registering and processing tweet id " ++
                                show originalTweetId ++
                                " from " ++ unpack userName ++
                                " text: " ++ unpack txt

                       registerTweet conn originalTweetId

                       spoonerizedText <- spoonerize $ unpack txt
                       case spoonerizedText of
                         Just value -> do
                           let textWithUserMention =
                                   replaceRunnyBabbotMention value
                                       (unpack userName)

                           let tweetResponse = TweetResponse
                                               { status = textWithUserMention
                                               , in_reply_to_status_id =
                                                   originalTweetId }

                           putStrLn $ "Responding to Tweet: " ++
                                    unpack txt ++ " with response: " ++
                                    show tweetResponse

                           postTweetResponse creds tweetResponse

                         Nothing -> putStrLn $ "Unspoonerizable tweet id " ++
                                    show originalTweetId ++
                                   ", only registering."



processNewTweets :: IConnection conn => conn -> OauthCredentials -> [Tweet] ->
                    IO ()
processNewTweets conn creds = mapM_ (processTweetResponse conn creds)

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
