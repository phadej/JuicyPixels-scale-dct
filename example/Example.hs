module Main (main) where

import Codec.Picture          (convertRGBA8, readImage, writePng)
import Codec.Picture.ScaleDCT (scale)
import Data.Time              (diffUTCTime, getCurrentTime)

processImage
    :: String      -- ^ From
    -> (Int, Int)  -- ^ Target size
    -> String      -- ^ To
    -> IO ()
processImage src size dst = do
    start <- getCurrentTime
    Right dimg <- readImage src
    let img = convertRGBA8 dimg
    let ava = scale size img
    writePng dst ava
    end <- getCurrentTime
    putStrLn $ dst ++ ": " ++ show (end `diffUTCTime` start)

main :: IO ()
main = do
    processImage "cat.jpg" (96, 64) "cat-small.png"
    processImage "cat.jpg" (769, 512) "cat-large.png"
    processImage "phadej.png" (64, 64) "phadej-small.png"
    processImage "phadej.png" (600, 600) "phadej-large.png"
