{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Server.Assets (favicons, frontend, frontendMin)
import Links (SiteLinks)

import LocalCooking.Server (LocalCookingArgs (..))
import LocalCooking.Colors (LocalCookingColors (..))
import LocalCooking.Dependencies.Chef (chefDependencies)
import LocalCooking.Dependencies.Tag (tagDependencies)

import Text.Lucius (Color (..))


server :: LocalCookingArgs SiteLinks sec
server = LocalCookingArgs
  { localCookingArgsFrontend = frontend
  , localCookingArgsFrontendMin = frontendMin
  , localCookingArgsFavicons = favicons
  , localCookingArgsHTTP = httpServer
  , localCookingArgsDeps = do
      chefDependencies
      tagDependencies
  , localCookingArgsColors = LocalCookingColors
    { localCookingColorsMain = Color 0 60 143
    , localCookingColorsActive = Color 21 101 192
    , localCookingColorsHover = Color 94 146 243
    }
  }
