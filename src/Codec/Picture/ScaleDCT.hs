{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Code.Picture.ScaleDCT
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Scale pictures using Discrete Cosine Transform.
--
module Codec.Picture.ScaleDCT (scale) where

import Codec.Picture (Image (..), PixelRGBA8 (..), generateImage, imagePixels)
-- TODO: we don't really need dependency on 'lens'
import Control.Lens.Fold (toListOf)
import Data.Array.CArray (CArray, amap, array, bounds, elems, listArray, size,
                          (!))
import Data.Ix           (inRange, range)
import Data.Word         (Word8)
import Math.FFT          (dct2N, dct3N)

type Array2D = CArray (Int, Int) Double

-- | Scale the image using DCT transform.
scale :: (Int, Int)        -- ^ Width, height
      -> Image PixelRGBA8  -- ^ Input image
      -> Image PixelRGBA8  --
scale dim img = fromChannels r' g' b' a'
  where
    r = channelR img
    g = channelG img
    b = channelB img
    a = channelA img

    transform ch = amap (k*) ch'
      where
        ch' = dct3N [1, 0] . cut dim . dct2N [0, 1] $ ch
        k = imgNorm ch / imgNorm ch'

    r' = transform r
    g' = transform g
    b' = transform b
    a' = transform a

imgNorm :: Array2D -> Double
imgNorm ch = sqrt . (/n) . sum . fmap sq . elems $ ch
  where sq x = x * x
        n = fromIntegral $ size ch

cut :: (Int, Int) -> Array2D -> Array2D
cut (w, h) img = array b [ (i, pick i) | i <- range b ]
  where b      = ((0,0), (h-1, w-1))
        b'     = bounds img
        pick i | inRange b' i = img ! i
               | otherwise    = 0

pixelR, pixelG, pixelB, pixelA :: PixelRGBA8 -> Word8
pixelR (PixelRGBA8 r _ _ _) = r
pixelG (PixelRGBA8 _ g _ _) = g
pixelB (PixelRGBA8 _ _ b _) = b
pixelA (PixelRGBA8 _ _ _ a) = a

extractChannel :: (PixelRGBA8 -> Word8) -> Image PixelRGBA8 -> Array2D
extractChannel f img@(Image w h _) = listArray ((0, 0), (h - 1, w - 1))
                                   . map (fromInteger . toInteger . f)
                                   . toListOf imagePixels
                                   $ img

channelR, channelG, channelB, channelA :: Image PixelRGBA8 -> Array2D
channelR = extractChannel pixelR
channelG = extractChannel pixelG
channelB = extractChannel pixelB
channelA = extractChannel pixelA

fromChannels :: Array2D
             -> Array2D
             -> Array2D
             -> Array2D
             -> Image PixelRGBA8
fromChannels r g b a = generateImage f w h
  where f x y = PixelRGBA8 (f' r) (f' g) (f' b) (f' a)
          where i = (y, x)
                f' c = truncate (limit $ c ! i)
        (_, (h', w')) = bounds r
        w = w' + 1
        h = h' + 1

limit :: Double -> Double
limit x | x < 0     = 0
        | x > 255   = 255
        | otherwise = x


