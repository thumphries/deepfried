{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DeepFried.Error (
    DeepFriedError (..)
  , renderDeepFriedError
  , EnvError (..)
  , renderEnvError
  ) where


import           BasicPrelude


data DeepFriedError
  = EnvError EnvError
  deriving (Eq, Show)

renderDeepFriedError :: DeepFriedError -> Text
renderDeepFriedError dfe = case dfe of
  EnvError e -> renderEnvError e

data EnvError
  = RequiredEnv Text
  deriving (Eq, Show)

renderEnvError :: EnvError -> Text
renderEnvError ee = case ee of
  RequiredEnv v ->
    "Missing ENV variable " <> v <> " is mandatory"
