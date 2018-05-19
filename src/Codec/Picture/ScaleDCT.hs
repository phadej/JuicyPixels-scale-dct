{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
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
module Codec.Picture.ScaleDCT (scale, scaleWithKernel) where

import Prelude ()
import Prelude.Compat

import Codec.Picture
       (Image (..), PixelRGBA8 (..), Traversal, generateImage, imagePixels)
import Control.Applicative (Const (..))
import Data.Array.CArray
       (CArray, amap, array, bounds, elems, listArray, size, (!))
import Data.Coerce         (Coercible, coerce)
import Data.Ix             (inRange, range)
import Data.Monoid         (Endo (..))
import Data.Word           (Word8)
import Math.FFT            (dct1N, dct2N, dct3N)

type Array2D = CArray (Int, Int) Double

-- | Scale the image using DCT transform.
scale
    :: (Int, Int)        -- ^ Output width, height
    -> Image PixelRGBA8  -- ^ Input image
    -> Image PixelRGBA8  -- ^ Output image
scale dim = scaleImpl (cut dim)

-- | Scale the image using DCT transform.
--
-- Convolute /result/ image with a symmetric kernel.
-- See <https://en.wikipedia.org/wiki/Symmetric_convolution>
--
-- Identity kernel:
--
-- @
-- k 0 0 = 1
-- k _ _ = 0
-- @
--
-- Sharpen:
--
-- @
-- k 0 0 = 1.75
-- k 0 1 = -0.125
-- k 1 0 = -0.125
-- k 1 1 = -0.0625
-- k _ _ = 0
-- @
--
scaleWithKernel
    :: (Int, Int)              -- ^ Output width, height
    -> (Int -> Int -> Double)  -- ^ kernel
    -> Image PixelRGBA8        -- ^ Input image
    -> Image PixelRGBA8        -- ^ Output image
scaleWithKernel dim@(w,h) kf = scaleImpl (cutWithKernel dim ker)
  where
    ker :: Array2D
    ker = dct1N [0, 1] $ array kb [ (i, kf x y) | i@(y, x) <- range kb ]
    kb = ((0, 0), (h-1, w-1))

scaleImpl :: (Array2D -> Array2D) -> Image PixelRGBA8 -> Image PixelRGBA8
scaleImpl cutImpl img = fromChannels r' g' b' a'
  where
    r = channelR img
    g = channelG img
    b = channelB img
    a = channelA img

    transform ch = amap (k*) ch'
      where
        ch' = dct3N [1, 0] . cutImpl . dct2N [0, 1] $ ch
        k = imgNorm ch / imgNorm ch'

    r' = transform r
    g' = transform g
    b' = transform b
    a' = transform a

imgNorm :: Array2D -> Double
imgNorm ch = sqrt . (/n) . sum . fmap sq . elems $ ch
  where
    sq x = x * x
    n = fromIntegral $ size ch

cut :: (Int, Int) -> Array2D -> Array2D
cut (w, h) img = array b [ (i, pick i) | i <- range b ]
  where
    b      = ((0,0), (h-1, w-1))
    b'     = bounds img
    pick i | inRange b' i = img ! i
           | otherwise    = 0

cutWithKernel :: (Int, Int) -> Array2D -> Array2D -> Array2D
cutWithKernel (w, h) k img = array b [ (i, pick i) | i <- range b ]
  where
    b      = ((0,0), (h-1, w-1))
    b'     = bounds img
    pick i | inRange b' i = k' i * (img ! i)
           | otherwise    = 0

    kb = bounds k
    k' i | inRange kb i = k ! i
         | otherwise    = 0

pixelR, pixelG, pixelB, pixelA :: PixelRGBA8 -> Word8
pixelR (PixelRGBA8 r _ _ _) = r
pixelG (PixelRGBA8 _ g _ _) = g
pixelB (PixelRGBA8 _ _ b _) = b
pixelA (PixelRGBA8 _ _ _ a) = a

extractChannel :: (PixelRGBA8 -> Word8) -> Image PixelRGBA8 -> Array2D
extractChannel f img@(Image w h _)
    = listArray ((0, 0), (h - 1, w - 1))
    . map (fromInteger . toInteger . f)
    . toListOf imagePixels
    $ img

channelR, channelG, channelB, channelA :: Image PixelRGBA8 -> Array2D
channelR = extractChannel pixelR
channelG = extractChannel pixelG
channelB = extractChannel pixelB
channelA = extractChannel pixelA

fromChannels
    :: Array2D
    -> Array2D
    -> Array2D
    -> Array2D
    -> Image PixelRGBA8
fromChannels r g b a = generateImage f w h
  where
    f x y = PixelRGBA8 (f' r) (f' g) (f' b) (f' a)
      where
        i = (y, x)
        f' c = truncate (limit $ c ! i)
    (_, (h', w')) = bounds r
    w = w' + 1
    h = h' + 1

limit :: Double -> Double
limit x | x < 0     = 0
        | x > 255   = 255
        | otherwise = x

-- From 'Control.Lens.Lens' from 'lens' package
toListOf :: Traversal s s a a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

foldrOf :: Traversal s s a a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

foldMapOf :: Monoid r => Traversal s s a a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)
{-# INLINE foldMapOf #-}

(#.) :: Coercible c b => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
{-# INLINE (#.) #-}

--(.#) :: Coercible b a => (b -> c) -> (a -> b) -> a -> c
--(.#) pbc _ = coerce pbc
