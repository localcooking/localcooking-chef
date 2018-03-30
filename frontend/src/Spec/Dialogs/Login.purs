module Spec.Dialogs.Login where

import Types.Env (env)

import Window (WindowSize (..), initialWindowSize)
import Links (SiteLinks (..), ThirdPartyLoginReturnLinks (..), toLocation, initSiteLinks)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)
import Facebook.State (FacebookLoginState (..))
import LocalCooking.Common.Password (HashedPassword, hashPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Time.Duration (Milliseconds (..))
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal
import React.Icons (facebookIcon, twitterIcon, googleIcon)

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Dialog (dialog)
import MaterialUI.Dialog as Dialog
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.TextField (textField)
import MaterialUI.TextField as TextField
import MaterialUI.Input as Input
import Crypto.Scrypt (SCRYPT)

import Queue.One (READ, Queue)
import IxSignal.Internal (IxSignal)
import Unsafe.Coerce (unsafeCoerce)



type State =
  { open :: Boolean
  , windowSize :: WindowSize
  , currentPage :: SiteLinks
  , email :: String
  , password :: String
  , pending :: Boolean
  }


initialState :: State
initialState =
  { open: false
  , windowSize: unsafePerformEff initialWindowSize
  , currentPage: initSiteLinks
  , email: ""
  , password: ""
  , pending: false
  }


data Action
  = Open
  | Close
  | ChangedWindowSize WindowSize
  | ChangedPage SiteLinks
  | ChangedEmail String
  | ChangedPassword String
  | SubmitLogin

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  | eff)


spec :: forall eff
      . { toURI :: Location -> URI
        , login :: EmailAddress -> HashedPassword -> Aff (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {toURI,login} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> do
        void $ T.cotransform _ { open = false }
        liftBase $ delay $ Milliseconds 2000.0
        void $ T.cotransform _ { email = "", password = "" }
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      ChangedEmail e -> void $ T.cotransform _ { email = e }
      ChangedPassword p -> void $ T.cotransform _ { password = p }
      SubmitLogin -> do
        void $ T.cotransform _ { pending = true }
        case emailAddress state.email of
          Nothing -> pure unit -- FIXME bug out somehow?
          Just email -> do
            liftBase $ do
              hashedPassword <- hashPassword {salt: env.salt, password: state.password}
              login email hashedPassword
            performAction Close props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ let dialog' =
              if state.windowSize < Laptop
              then
                dialog
                  { open: state.open
                  , fullScreen: true
                  }
              else
                dialog
                  { open: state.open
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> dispatch Close
                  }
        in  dialog'
            [ dialogTitle {} [R.text "Login"]
            , dialogContent {}
              [ textField
                { label: R.text "Email"
                , fullWidth: true
                , onChange: mkEffFn1 \e -> dispatch $ ChangedEmail (unsafeCoerce e).target.value
                , error: case emailAddress state.email of
                  Nothing -> false
                  Just _ -> true
                }
              , textField
                { label: R.text "Password"
                , fullWidth: true
                , "type": Input.passwordType
                , onChange: mkEffFn1 \p -> dispatch $ ChangedPassword (unsafeCoerce p).target.value
                }
              , R.div [RP.style {display: "flex", justifyContent: "space-evenly", paddingTop: "2em"}] $
                  let mkFab mainColor darkColor icon mLink =
                        Button.withStyles
                          (\theme ->
                            { root: createStyles
                              { backgroundColor: mainColor
                              , color: "#ffffff"
                              , "&:hover": {backgroundColor: darkColor}
                              }
                            }
                          )
                          (\{classes} ->
                            button
                              { variant: Button.fab
                              , classes: Button.createClasses {root: classes.root}
                              , disabled: case mLink of
                                Nothing -> true
                                _ -> false
                              , href: case mLink of
                                Nothing -> ""
                                Just link -> URI.print $ facebookLoginLinkToURI link
                              } [icon]
                          )
                  in  [ mkFab "#3b5998" "#1e3f82" facebookIcon $
                         Just $ FacebookLoginLink
                         { redirectURL: toURI (toLocation FacebookLoginReturn)
                         , state: FacebookLoginState
                           { origin: state.currentPage
                           }
                         }
                      , mkFab "#1da1f3" "#0f8cdb" twitterIcon Nothing
                      , mkFab "#dd4e40" "#c13627" googleIcon Nothing
                      ]
              ]
            , dialogActions {}
              [ button
                { color: Button.secondary
                -- , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Register"]
              , button
                { color: Button.primary
                -- , onTouchTap: mkEffFn1 \_ -> dispatch Close
                , disabled: case emailAddress state.email of
                  Nothing -> true
                  Just _ -> false
                } [R.text "Submit"]
              , button
                { color: Button.default
                , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Cancel"]
              ]
            ]
      ]



loginDialog :: forall eff
             . { openLoginSignal :: Queue (read :: READ) (Effects eff) Unit
               , windowSizeSignal :: IxSignal (Effects eff) WindowSize
               , toURI :: Location -> URI
               , currentPageSignal :: IxSignal (Effects eff) SiteLinks
               , login :: EmailAddress -> HashedPassword -> Aff (Effects eff) Unit
               }
            -> R.ReactElement
loginDialog {openLoginSignal,windowSizeSignal,toURI,currentPageSignal,login} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec {toURI,login}) initialState
      reactSpecLogin =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
        $ Queue.whileMountedOne
            openLoginSignal
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
