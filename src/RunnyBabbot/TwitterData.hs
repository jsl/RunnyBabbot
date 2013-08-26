{-# LANGUAGE DeriveGeneric #-}

module RunnyBabbot.TwitterData
    ( Tweet(..)
    , User(..)
    , TweetResponse(..)
    ) where


import Data.Aeson
import GHC.Generics
import Data.Text (Text)

data User = User { screen_name :: !Text } deriving (Show, Generic)

data Tweet = Tweet
    { text :: !Text
    , id   :: Integer
    , user :: User
    } deriving (Show, Generic)

instance FromJSON User
instance FromJSON Tweet

data TweetResponse = TweetResponse
    { status :: String
    , in_reply_to_status_id :: Integer } deriving (Show, Eq)
