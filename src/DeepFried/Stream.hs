{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DeepFried.Stream (
    printUserstream
  , printTracking
  , favMentions
  ) where


import           BasicPrelude

import           Control.Lens
import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as CL

import           Web.Twitter.Conduit
import           Web.Twitter.Types (Status (..), StreamingAPI (..), User (..))



printUserstream :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => TWInfo -> Manager -> m ()
printUserstream twInfo mgr = runResourceT $ do
  src <- stream twInfo mgr userstream
  src $$+- CL.mapM_ (liftIO . print)

printTracking :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => TWInfo -> Manager -> Text -> m ()
printTracking twInfo mgr text = runResourceT $ do
  src <- stream twInfo mgr (statusesFilterByTrack text)
  src $$+- CL.mapM_ (liftIO . print)

favMentions :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => TWInfo -> Manager -> Text -> m ()
favMentions twInfo mgr uname = runResourceT $ do
  src <- stream twInfo mgr (statusesFilterByTrack uname)
  src $$+- CL.mapM_ (favStream twInfo mgr)

favStream :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => TWInfo -> Manager -> StreamingAPI -> m ()
favStream twInfo mgr event = case event of
  SStatus s -> do
    let user = userScreenName (statusUser s)
        reply = ".@" <> user
    f <- liftIO $ call twInfo mgr (favoritesCreate (statusId s))
    s <- liftIO $ call twInfo mgr (updateWithMedia reply (MediaFromFile "/tmp/sponge.jpg") & inReplyToStatusId ?~ (statusId s))
    print f
  _ ->
    pure ()

