module Main (main) where

import Codec.Picture          (readImage, writePng)
import Codec.Picture.RGBA8    (fromDynamicImage)
import Codec.Picture.ScaleDCT (scale)

main :: IO ()
main = do
    Right dimg <- readImage "phadej.png"
    let img = fromDynamicImage dimg
    let ava1 = scale (64, 64) img
    let ava2 = scale (600, 600) img
    writePng "phadej-small.png" ava1
    writePng "phadej-large.png" ava2
