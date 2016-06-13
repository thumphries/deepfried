{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DeepFried.Image (
    deepfry
  ) where


import           BasicPrelude

import           Graphics.GD

import           System.IO
import           System.Random


deepfry :: FilePath -> IO FilePath
deepfry input = do
  (out, h) <- openBinaryTempFileWithDefaultPermissions "/tmp" "deepfried.jpg"

  withImage (loadJpegFile input) $ \image -> do
    fried <- fry image
    saveJpegFile (-1) out fried

  hClose h
  pure out

fry :: Image -> IO Image
fry i = do
  iterations <- randomRIO (30, 100)
  iterateM iterations viaJpegRandomResize i

viaJpegRandomResize :: Image -> IO Image
viaJpegRandomResize i = do
  (x, y) <- imageSize i
  quality <- randomRIO (1, 50)
  percentX <- randomRIO (60, 95)
  percentY <- randomRIO (60, 95)
  j <- resizeImage (percent percentX x) (percent percentY y) i
  b <- saveJpegByteString quality j
  k <- loadJpegByteString b
  resizeImage x y k

percent :: Int -> Int -> Int
percent per n = ceiling $ (fromIntegral (per * n) * 0.01 :: Double)

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n-1) f
