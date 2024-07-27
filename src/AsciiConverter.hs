module AsciiConverter
    ( 
      
loadAndConvertImageFromDataURL
    ) where

import Codec.Picture
import qualified Data.ByteString as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (pack, unpack)
import Data.Either (either)

asciiChars :: String
asciiChars = "`'.,^:;Il!i~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"

pixelToGray :: PixelRGB8 -> Pixel8
pixelToGray (PixelRGB8 r g b) = round $ (fromIntegral (r + g + b) :: Double) / 3

grayToAscii :: Pixel8 -> Char
grayToAscii gray = asciiChars !! index
  where
    index = round $ (fromIntegral gray / 255.0) * (fromIntegral (length asciiChars) - 1 :: Double)

convertImageToAscii :: DynamicImage -> String
convertImageToAscii (ImageRGB8 img) = unlines [ [ pixelToAscii x y | x <- [0..imageWidth img - 1] ] | y <- [0..imageHeight img - 1] ]
  where
    pixelToAscii x y = grayToAscii . pixelToGray $ pixelAt img x y
convertImageToAscii _ = "Unsupported image format."

convertToRGB8 :: DynamicImage -> Maybe (Image PixelRGB8)
convertToRGB8 (ImageRGB8 img) = Just img
convertToRGB8 (ImageRGBA8 img) = Just $ pixelMap dropAlpha img
convertToRGB8 (ImageYCbCr8 img) = Just $ convertRGB8 (ImageYCbCr8 img)
convertToRGB8 _ = Nothing

dropAlpha :: PixelRGBA8 -> PixelRGB8
dropAlpha (PixelRGBA8 r g b _) = PixelRGB8 r g b

resizeImage :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
resizeImage targetWidth targetHeight img = generateImage pixelMapper targetWidth targetHeight
  where
    originalWidth = imageWidth img
    originalHeight = imageHeight img
    scaleX :: Double
    scaleX = fromIntegral originalWidth / fromIntegral targetWidth
    scaleY :: Double
    scaleY = fromIntegral originalHeight / fromIntegral targetHeight
    pixelMapper x y = pixelAt img (floor $ scaleX * fromIntegral x) (floor $ scaleY * fromIntegral y)

loadAndConvertImage :: B.ByteString -> Either String String
loadAndConvertImage bs = 
    case decodeImage bs of
        Left err -> Left $ "Error loading image: " ++ err
        Right dynImg -> case convertToRGB8 dynImg of
            Just img -> do
              let resizedImg = resizeImage 80 40 img
              Right $ convertImageToAscii (ImageRGB8 resizedImg)
            Nothing -> Left "Unsupported image format after conversion."

loadAndConvertImageFromDataURL :: String -> Either String String
loadAndConvertImageFromDataURL dataUrl =
  case break (==',') dataUrl of
    (_, ',' : base64Data) ->
      case B64.decode (pack base64Data) of
        Left err -> Left $ "Base64 decoding error: " ++ err
        Right bs -> loadAndConvertImage bs
    _ -> Left "Invalid data URL format."
