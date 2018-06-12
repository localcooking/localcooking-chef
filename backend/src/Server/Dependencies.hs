{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import LocalCooking.Dependencies.Chef (chefDependencies)
import LocalCooking.Function.System (SystemM)

import Web.Routes.Nested (l_, o_, (</>))
import Web.Dependencies.Sparrow (SparrowServerT, matchGroup)
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT SystemM) [] SystemM ()
dependencies =
  chefDependencies
