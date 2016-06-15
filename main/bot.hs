{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           BasicPrelude hiding (first)

import           Control.Concurrent

import qualified Data.Text.IO as T

import           DeepFried.Env
import           DeepFried.Error
import           DeepFried.Stream

import           Web.Twitter.Conduit

import           X.Control.Monad.Trans.Either
import           X.Control.Monad.Trans.Either.Exit


main :: IO ()
main = orDie renderDeepFriedError $ do
  oauth <- firstEitherT EnvError oauthEnv
  creds <- firstEitherT EnvError credsEnv
  let twInfo = setCredential oauth creds def
  mgr <- liftIO (newManager tlsManagerSettings)

  liftIO $ T.putStrLn "deepfried starting"

  -- FIX squash exceptions and run in a loop
  _ <- liftIO . forkIO $ favTrack twInfo mgr ["deep fried memes", "deep fried meme"]
  -- FIX should be able to get screen name via API
  liftIO $ fryMentions twInfo mgr "deepfrybot"
