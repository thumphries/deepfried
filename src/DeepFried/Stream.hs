{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DeepFried.Stream (
    favTrack
  , onMentions
  , fryMentions
  ) where


import           BasicPrelude

import           Control.Lens
import           Control.Monad.Trans.Resource

import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T

import           DeepFried.Image

import           Network.HTTP.Client (RequestBody (..), parseUrl)
import           Network.HTTP.Simple (getResponseBody, httpLBS)

import           Web.Twitter.Conduit
import           Web.Twitter.Types (Entity (..))
import           Web.Twitter.Types.Lens hiding (entityBody)


-- fav every tweet in this tracking stream
favTrack :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
         => TWInfo -> Manager -> [Text] -> m ()
favTrack twInfo mgr tracking = runResourceT $ do
  src <- stream twInfo mgr (statusesFilter [Track tracking])
  src $$+- CL.mapM_ (favStream twInfo mgr)

favStream :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => TWInfo -> Manager -> StreamingAPI -> m ()
favStream twInfo mgr event = case event of
  SStatus s -> do
    s2 <- liftIO $ call twInfo mgr (favoritesCreate (s ^. statusId))
    print (s2 ^. statusId)
  _ ->
    pure ()


-- dodgy hack: stream mentions by running a search for screenname
onMentions :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
           => TWInfo -> Manager -> Text
           -> (StreamingAPI -> m ())
           -> m ()
onMentions twInfo mgr uname k = runResourceT $ do
  src <- stream twInfo mgr (statusesFilterByTrack uname)
  src $$+- CL.mapM_ (lift . k)

fryMentions :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
            => TWInfo -> Manager -> Text -> m ()
fryMentions twInfo mgr uname = onMentions twInfo mgr uname $ \sapi -> do
  case sapi of
    SStatus s -> do
      let usern = s ^. statusUser ^. userScreenName
          reply = "@" <> usern
      maybe
        (pure ())
        (\r -> do
          image <- liftM (LB.toStrict . getResponseBody) (httpLBS r)
          fried <- liftIO (deepfryBS image)
          s2 <- liftIO . call twInfo mgr
            $ updateWithMedia reply (MediaRequestBody "fried.jpg" (RequestBodyBS fried))
              & inReplyToStatusId ?~ (s ^. statusId)
          liftIO $ print (s2 ^. statusId))
        (statusMedia s >>= listToMaybe >>= parseUrl . T.unpack)
    _ ->
      pure ()

statusMedia :: Status -> Maybe [URIString]
statusMedia s =
  liftM (mapMaybe (unUri . entityBody) . (^. enMedia)) (s ^. statusEntities)

unUri :: MediaEntity -> Maybe URIString
unUri e =
  case e ^. meType of
    "photo" -> pure (e ^. meMediaURL)
    _ -> mempty
