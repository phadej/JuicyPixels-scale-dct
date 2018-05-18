module Main (main) where

import Codec.Picture          (convertRGBA8, readImage, writePng)
import Codec.Picture.ScaleDCT (scale, scaleWithKernel)
import Data.Time              (diffUTCTime, getCurrentTime)

processImage
    :: String      -- ^ From
    -> (Int, Int)  -- ^ Target size
    -> String      -- ^ To
    -> Maybe (Int -> Int -> Double) -- ^ kernel
    -> IO ()
processImage src size dst mk = do
    let scale' = maybe (scale size) (scaleWithKernel size) mk
    start <- getCurrentTime
    eimg <- readImage src
    case eimg of
        Left err -> print err
        Right dimg -> do
            let img = convertRGBA8 dimg
            let ava = scale' img
            writePng dst ava
            end <- getCurrentTime
            putStrLn $ dst ++ ": " ++ show (end `diffUTCTime` start)


main :: IO ()
main = do
    processImage "cat.jpg" (96, 64) "cat-small.png" Nothing
    processImage "cat.jpg" (769, 512) "cat-large.png" Nothing
    processImage "phadej.png" (32, 32) "phadej-smaller.png" (Just k)
    processImage "phadej.png" (64, 64) "phadej-small.png" (Just k)
    processImage "phadej.png" (600, 600) "phadej-large.png" Nothing
  where
    k 0 0 = 2.75 -- make this larger or smaller to adjust amount of an effect.
    k 0 1 = -0.125
    k 1 0 = -0.125
    k 1 1 = -0.0625
    k _ _ = 0
