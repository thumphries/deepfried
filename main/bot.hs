{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           BasicPrelude hiding (first)

import           DeepFried.Env
import           DeepFried.Error
import           DeepFried.Stream
import           DeepFried.Image

import           Web.Twitter.Conduit

import           X.Control.Monad.Trans.Either
import           X.Control.Monad.Trans.Either.Exit


main :: IO ()
main = orDie renderDeepFriedError $ do
  oauth <- firstEitherT EnvError oauthEnv
  creds <- firstEitherT EnvError credsEnv
  let twInfo = setCredential oauth creds def
  mgr <- liftIO (newManager tlsManagerSettings)

  printUserstream twInfo mgr
  -- printTracking twInfo mgr "Haskell"

  -- favMentions twInfo mgr "@deepfrybot"

  pure ()
