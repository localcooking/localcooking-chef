module Spec.Content.Root where

import Links (AboutPageLinks (..))

import Prelude
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (toLocation, Location)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (div, em, img, strong, text) as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import DOM.HTML.Window.Extra (WindowSize (Laptop))

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State =
  { windowSize :: WindowSize
  }

initialState :: {initWindowSize :: WindowSize} -> State
initialState {initWindowSize} =
  { windowSize: initWindowSize
  }

data Action
  = ChangedWindowSize WindowSize

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)


spec :: forall eff
      . { toURI :: Location -> URI
        }
     -> T.Spec eff State Unit Action
spec {toURI} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: if state.windowSize < Laptop then Typography.headline else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em"}
        } [R.text "Build Your Own Menus, Sell Your Own Creations"]
      ] <> ( if state.windowSize < Laptop
                then paragraph1
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid {xs: 8, item: true} $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph1 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    , grid {xs: 4, item: true}
                      [ R.img
                        [ RP.src $ URI.print $ toURI $ toLocation Paragraph1Png
                        , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0.2em"}
                        ] []
                      ]
                    ]
                  ]
           ) <>
      [ divider {}
      , typography
        { variant: if state.windowSize < Laptop then Typography.headline else Typography.display1
        , align: Typography.left
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "Work Your Own Schedule, Manage Your Own Orders"]
      ] <> ( if state.windowSize < Laptop
                then paragraph2
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid {xs: 4, item: true}
                      [ R.img
                        [ RP.src $ URI.print $ toURI $ toLocation Paragraph2Png
                        , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0.2em"}
                        ] []
                      ]
                    , grid {xs: 8, item: true} $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph2 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    ]
                  ]
           ) <>
      [ divider {}
      , typography
        { variant: if state.windowSize < Laptop then Typography.headline else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "Develop Your Own Portfolio And Local Reputation"]
      ] <> ( if state.windowSize < Laptop
                then paragraph3
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid
                      { xs: 8
                      , item: true
                      } $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph3 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    , grid {xs: 4, item: true}
                      [ R.img
                        [ RP.src $ URI.print $ toURI $ toLocation Paragraph3Png
                        , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0.2em"}
                        ] []
                      ]
                    ]
                  ]
           )


root :: forall eff
      . { windowSizeSignal :: IxSignal (Effects eff) WindowSize
        , toURI :: Location -> URI
        }
     -> R.ReactElement
root {windowSizeSignal,toURI} =
  let init =
        { initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} = T.createReactSpec (spec {toURI}) (initialState init)
      reactSpec' =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []


paragraph1 :: Array R.ReactElement
paragraph1 =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    , paragraph: true
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Building and managing a website is hard, but in our modern age, it's a necessary requirement for any serious small business or craftsman. Our platform takes care of the technical details by giving our chefs an "
    , R.em [] [R.text "interactive menu"]
    , R.text " editing system; each chef crafts "
    , R.strong [] [R.text "their own"]
    , R.text " menus and recipes, using ingredients locally available. — search for a specific dish, or for a style of talent."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , paragraph: true
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Every menu is modern and richly interactive — chefs can publish a near unlimited amount of detail per meal, giving rise to various multimedia accompanied with each meal description — from close-ups of a finished product, to preparation instructions, we have an open playing field for our chefs to express their talent."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Local Cooking chefs are independent contractors, and get paid a majority commission per-order; Local Cooking just manages the insurance of wholesale produce orders and kitchen supplies & upkeep.Each chef is responsible for their customer base and supplying market demands."
    ]
  ]


-- FIXME links!!
paragraph2 :: Array R.ReactElement
paragraph2 =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    , paragraph: true
    }
    [ R.text "We want to make working for Local Cooking a pleasure; our customer market is not based on immediate time constraints, so neither should our chefs. Our schedule reservation interface allows chefs to schedule their work around their dynamic life."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    , paragraph: true
    }
    [ R.text "Reservations to work in the kitchen are first-come-first-serve, but wait listing and schedule bartering is built-in: no more hassling with managers to get the hours you want."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Because chefs work their own schedules, they are expected to meet their obligations independently; however, Local Cooking is a meal insurance company, and we will verify that our chefs are on-schedule, or close to it. We refrain from micromanagement — so long as your practices aren't questionable and you're getting along with the other chefs, you are free to do what you need when you need to."
    ]
  ]


paragraph3 :: Array R.ReactElement
paragraph3 =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    , paragraph: true
    }
    [ R.text "The traditional road to culinary stardom is paved with monumentally excessive labor, mostly stemming from the inability to prove your reliable talent."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    , paragraph: true
    }
    [ R.text "Our platform tracks orders and has a sophisticated review system — each chef's menus, order history, and customer feedback is automatically included in their online portfolio; we recognize chefs who work for us may have other long-term interests than Local Cooking, and we want to support that. Every chef's profile belongs to them after employment."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "We also feature an apprenticeship program, and a sub-contracting tool in our platform — we want to employ useful cooks wherever they fit best; if menu design isn't your thing, we have plenty of routine prep work. Likewise, if you want to improve your skills, we can pair you with a more senior level chef of your choosing."
    ]
  ]
