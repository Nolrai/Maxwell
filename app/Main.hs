{-# LANGUAGE NoImplicitPrelude #-}
module Main (main, myImage) where
import Graphics.Image as Image
    ( makeImageR, writeImage, RGB, Image, Pixel (PixelRGB), RPU(..), toWord8Px)
import Graphics.Image.Interface (Elevator)
import ImageFunction (imageFunctions)
import Relude
import System.Directory

main :: IO ()
main = do
  createDirectoryIfMissing True prefix
  args <- getArgs
  let resolution = fromMaybe 30 $ readMaybe =<< viaNonEmpty head args 
  sequence_ $ do
    outer <- [doImageCurved, doImageFlat, doLogFlat, doFindMaxCurved]
    outer resolution <$> imageFunctions

doImage, doLog :: 
  String -> 
  (Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double) -> 
  Int ->
  (String, Double -> Double -> Pixel Image.RGB Double) ->
  IO ()
doImage suffix rows resolution (name, function ) = writeImage (prefix <> name <> suffix) (myImage resolution rows function)
doLog suffix rows resolution (name, function) = 
  writeFileText (prefix <> name <> suffix) . unlines . (show <$>) $ pixelList resolution rows function

findMax :: MonadIO m =>
  String
  -> (Int -> f -> (Int, Int) -> Pixel RGB Double)
  -> Int
  -> (String, f)
  -> m ()
findMax suffix rows resolution (name, function) =
  [("red", toRed), ("green", toGreen), ("blue", toBlue)] `forM_`
    \ (colorName, f) -> do
        writeFileText (prefix <> name <> "_" <> colorName <> "_" <> suffix)
          . unlines 
          . (show <$>)
          . sortBy (compare `on` f)
          $ pixelList resolution rows function
  where
    toRed   (_, PixelRGB r  _g _b) = r
    toGreen (_, PixelRGB _r  g _b) = g
    toBlue  (_, PixelRGB _r _g  b) = b
  
pixelList :: Elevator e => Int -> (Int -> f -> (Int, Int) -> Pixel RGB e) -> f -> [((Int, Int), Pixel RGB Word8)]
pixelList resolution rows function =
      (\ x y -> ((x, y), toWord8Px $ rows resolution function (x,y))) <$> [0 .. resolution - 1] <*> [0 .. resolution - 1]

prefix :: String
prefix = "./pictures/"

doImageSingleton, doImageCurved, doImageAlternating, doImageFlat, doLogFlat, doFindMaxCurved :: Int -> (String, Double -> Double -> Pixel RGB Double) -> IO ()
doImageAlternating  = doImage "_alternating.png"  shiftAlernateRows
doImageCurved       = doImage "_curved.png"       curvedRows
doImageFlat         = doImage "_flat.png"         flatRows
doImageSingleton    = doImage "_singleton.png"    singletonRows
doLogFlat           = doLog   "_flat.log"         flatRows
doFindMaxCurved     = findMax "_curved_maximums.log" curvedRows

myImage :: 
  Int -> 
  (Int -> (Double -> Double -> Pixel RGB Double) -> (Int, Int) -> Pixel RGB Double) ->
  (Double -> Double -> Pixel Image.RGB Double) -> 
  Image RPU Image.RGB Double
myImage size rows f = makeImageR RPU (size, size) (rows size f)

shifted :: (Integral a) => a -> a -> a
shifted size x = if x > size' then x - size' else x + size'
  where
    size' = size `div` 2

singletonRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
singletonRows size imageFunction (x, _) = imageFunction (1/2) x'
  where
  x' :: Double
  x' = fromIntegral (shifted size x) / fromIntegral size

flatRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
flatRows size imageFunction (x, y) = imageFunction y' x'
  where
  x' = fromIntegral x / fromIntegral size
  y' = fromIntegral y / fromIntegral size

curvedRowsShifted' :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
curvedRowsShifted' size imageFunction (x, y) = curvedRows size imageFunction (shifted size x, y)

curvedRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
curvedRows size imageFunction (x, y) = 
    if 0 <= x' && x' <= 1
      then imageFunction y' x'
      else imageFunction 0 0
  where
  x' = (((fromIntegral x / fromIntegral size) - (1/2)) / sin (y' * pi)) + 1/2
  y' = fromIntegral y / fromIntegral size

shiftAlernateRows :: Int -> (Double -> Double -> Pixel Image.RGB Double) -> (Int, Int) -> Pixel Image.RGB Double
shiftAlernateRows size imageFunction (x, y) = flatRows size imageFunction (x, y')
  where
    y' :: Int
    y' = if even x then y else shifted (size `div` 2) y