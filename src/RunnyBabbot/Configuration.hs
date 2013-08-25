module RunnyBabbot.Configuration (OauthCredentials(..),
                                  credentials)
where

import Data.ConfigFile
import qualified Data.ByteString.Char8 as BLC (ByteString)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error as MTL (runErrorT, join)

data OauthCredentials = OauthCredentials {
      consumerKey       :: BLC.ByteString,
      consumerSecret    :: BLC.ByteString,
      accessToken       :: BLC.ByteString,
      accessTokenSecret :: BLC.ByteString
    } deriving (Show)

credentials :: String -> IO (Either CPError OauthCredentials)
credentials filepath = do
  cred <- MTL.runErrorT $
        do
          cp <- join $ liftIO $ readfile emptyCP filepath
          consumerKey       <- get cp "oauth" "consumer_key"
          consumerSecret    <- get cp "oauth" "consumer_secret"
          accessToken       <- get cp "oauth" "access_token"
          accessTokenSecret <- get cp "oauth" "access_token_secret"
          return (OauthCredentials consumerKey consumerSecret
                                   accessToken accessTokenSecret)
  return cred
