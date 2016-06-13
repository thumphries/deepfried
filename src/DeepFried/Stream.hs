{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DeepFried.Stream (
    printUserstream
  ) where


import           BasicPrelude

import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as CL

import           Web.Twitter.Conduit as Twitter


printUserstream :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => TWInfo -> Manager -> m ()
printUserstream twInfo mgr = runResourceT $ do
  src <- stream twInfo mgr Twitter.userstream
  src $$+- CL.mapM_ (liftIO . print)
