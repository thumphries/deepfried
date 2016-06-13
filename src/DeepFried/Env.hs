{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DeepFried.Env (
    oauthEnv
  , credsEnv
  ) where


import           BasicPrelude

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import           DeepFried.Error

import           System.Environment (lookupEnv)

import           Web.Twitter.Conduit

import           X.Control.Monad.Trans.Either


oauthEnv :: EitherT EnvError IO OAuth
oauthEnv = do
  key <- requireEnv "OAUTH_CONSUMER_KEY"
  secret <- requireEnv "OAUTH_CONSUMER_SECRET"
  pure twitterOAuth {
      oauthConsumerKey = key
    , oauthConsumerSecret = secret
    }

credsEnv :: EitherT EnvError IO Credential
credsEnv = do
  token <- requireEnv "OAUTH_ACCESS_TOKEN"
  secret <- requireEnv "OAUTH_ACCESS_SECRET"
  pure $ Credential [
      ("oauth_token", token)
    , ("oauth_token_secret", secret)
    ]

requireEnv :: Text -> EitherT EnvError IO ByteString
requireEnv var = do
  mt <- lift $ lookupEnv (T.unpack var)
  maybe (left (RequiredEnv var)) (pure . B8.pack) mt
