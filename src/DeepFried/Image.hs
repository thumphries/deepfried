{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DeepFried.Image (
    deepfryFile
  , deepfryBS
  , FryScore (..)
  , renderFryScore
  ) where


import           BasicPrelude

import           Graphics.GD

import           System.IO
import           System.Random


data FryScore
  = Legible Int
  | GoldenBrown Int
  | Crispy Int
  deriving (Eq, Show)

fryScore :: Int -> FryScore
fryScore i
  | i <= 40 = Legible i
  | i <= 70 = GoldenBrown i
  | otherwise = Crispy i

renderFryScore :: FryScore -> Text
renderFryScore f = case f of
  Legible i -> "still legible! [" <> show i <> "]"
  GoldenBrown i -> "i cook it golden brown [" <> show i <> "]"
  Crispy i -> "crispy [" <> show i <> "]"

deepfryFile :: FilePath -> IO (FilePath, FryScore)
deepfryFile input = do
  (out, h) <- openBinaryTempFileWithDefaultPermissions "/tmp" "deepfried.jpg"

  s <- withImage (loadJpegFile input) $ \image -> do
    (fried, s) <- fry image
    saveJpegFile (-1) out fried
    pure s

  hClose h
  pure (out, s)

deepfryBS :: ByteString -> IO (ByteString, FryScore)
deepfryBS bs = do
  withImage (loadJpegByteString bs) $ \image -> do
    (fried, s) <- fry image
    bs <- saveJpegByteString 70 fried
    pure (bs, s)

fry :: Image -> IO (Image, FryScore)
fry i = do
  iterations <- randomRIO (20, 80)
  i <- iterateM iterations viaJpegRandomResize i
  pure (i, fryScore iterations)

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
