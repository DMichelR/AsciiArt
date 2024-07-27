{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Update where

import Miso
import Miso.String as MS hiding (length)
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.Foreign.Callback
import AsciiConverter (loadAndConvertImageFromDataURL)

data Action
  = LoadFile
  | NoOp
  | SetFile MisoString
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel LoadFile m = m <# do
  fileReaderInput <- getElementById "fileReader"
  file <- getFile fileReaderInput
  reader <- newReader
  mvar <- newEmptyMVar
  setOnLoad reader =<< do
    asyncCallback $ do
      r <- getResult reader
      putMVar mvar r
  readAsDataURL reader file
  SetFile <$> readMVar mvar
updateModel (SetFile dataUrl) m = noEff m { data = asciiArt }
  where
    asciiArt = case loadAndConvertImageFromDataURL (fromMisoString dataUrl) of
      Left err -> toMisoString err
      Right art -> toMisoString art
updateModel NoOp m = noEff m

foreign import javascript unsafe "console.log($1)"
  js_consoleLog :: JSString -> IO ()

foreign import javascript unsafe "$r = new FileReader();"
  newReader :: IO JSVal

foreign import javascript unsafe "$r = $1.files[0];"
  getFile :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.onload = $2;"
  setOnLoad :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.result;"
  getResult :: JSVal -> IO MisoString
  
foreign import javascript unsafe "$1.readAsDataURL($2);"
  readAsDataURL :: JSVal -> JSVal -> IO ()
