{-# LANGUAGE OverloadedStrings #-}

module View where

import Miso
import Miso.String as MS hiding (length)
import qualified Data.Map as Map

viewModel :: Model -> View Action
viewModel Model {..} = view
  where
    googleFontsLink = link_ [ rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Space+Mono" ]
    view =  div_ [] [googleFontsLink, div_ [ style_ $ Map.fromList [("font-family", "Space Mono"),("background", "#FFFFFF"), ("display", "flex"), ("flexDirection", "column"), ("alignItems", "center"), ("boxSizing", "border-box"), ("padding", "100px 0 100px 0")] ] [
              div_ [ style_ $ Map.fromList [("display", "flex"), ("flexDirection", "column"), ("alignItems", "center"), ("padding", "160px 0 160px 0")] ] [
                div_ [ style_ $ Map.fromList [("margin", "0 0 32px 0"), ("display", "flex"), ("flexDirection", "column"), ("alignItems", "center"), ("width", "fit-content"), ("boxSizing", "border-box")] ] [
                  div_ [ style_ $ Map.fromList [("text-shadow", "0px 4px 4px 0px rgba(0,0,0,0.25)"), ("margin", "0 0 8px 0"), ("display", "inline-block"), ("wordBreak", "break-word"), ("fontFamily", "Inter"), ("fontWeight", "700"), ("fontSize", "72px"), ("letterSpacing", "-2.2px"), ("lineHeight", "1.2"), ("color", "#1E1E1E")] ] [
                    text "Image to Ascii Art"
                  ]
                ],
                div_ [ style_ $ Map.fromList [("display", "flex"), ("width", "240px"), ("boxSizing", "border-box")] ] [
                    input_ [ id_ "fileReader", type_ "file", onChange (const LoadFile), style_ $ Map.fromList [("wordBreak", "break-word"), ("fontFamily", "Inter"), ("fontWeight", "400"), ("fontSize", "16px"), ("lineHeight", "1"), ("color", "#F5F5F5")]]
                ]
              ],
              div_ [ style_ $ Map.fromList [("opacity", "0.8"), ("background", "#FFFFFF"), ("display", "flex"), ("flexDirection", "row"), ("padding", "64px"), ("width", "900px"), ("boxSizing", "border-box")] ] [
                div_ [style_ $ Map.fromList [("margin-top", "20px")]] [ text data ]
              ]
            ]]
