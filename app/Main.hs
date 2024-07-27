{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import Miso
import Miso.String as MS hiding (length)
import Update (updateModel, Action(..))
import View (viewModel)

data Model
  = Model
  { data :: MisoString
  } deriving (Eq, Show)

main :: IO ()
main = do
  startApp App { model = Model {data = ""}
               , initialAction = NoOp
               , ..
               }
    where
      mountPoint = Nothing
      update = updateModel
      events = defaultEvents
      subs   = []
      view   = viewModel
      logLevel = Off
