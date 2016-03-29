module Home where

import String
import Util

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects
import Task
import Css exposing (..)
import Css.Elements as Css
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import ColorScheme exposing (..)
import Sass exposing (Symbol(..))
import CssValidator exposing (isValidElmCssFunction)


cssNamespace = "homepage"

{ class, classList, id } = Html.CssHelpers.withNamespace cssNamespace


type CssClasses =
  Content | Input | Output | Alias

css =
  (stylesheet << namespace cssNamespace)
    [ Css.body
        [ backgroundColor accent1 ]
    , (.) Content
        [ Css.width (px 960)
        , margin2 zero auto
        ]
    , each [ (.) Input, (.) Output ]
        [ Css.width (pct 40)
        , Css.height (px 500)
        , fontFamily monospace
        ]
    , aliasCss
    ]




viewOutput : List Sass.Symbol -> Html
viewOutput alias =
    let
        elmCss =
            alias
                |> Sass.toElmCss
    in
        textarea
            [ class [ Output ]
            , value elmCss
            ]
            [ text elmCss ]

viewInput : Signal.Address Action -> String -> Html
viewInput address input =
    textarea
        [ on "input" targetValue (Signal.message address << UpdateInput)
        , class [ Input ]
        , placeholder "Put a valid JSON object in here!"
        ]
        [ text <| input ]

viewErrors : List Sass.Symbol -> Html
viewErrors alias =
    let
        errors =
            alias
                |> Sass.allRules
                |> List.filterMap (\x ->
                    case x of
                        Rule rule ->
                            if isValidElmCssFunction rule.name then
                                Nothing
                            else
                                String.join ""
                                    [ "The function "
                                    , rule.name
                                    , " was not found for the rule "
                                    , rule.value
                                    , ".\nMaybe you meant: "
                                    , String.join ", " <| CssValidator.suggestions (CssValidator.names) rule.name
                                    ]
                                    |> Just
                        _ ->
                            Nothing
                    )
    in

        ul
            []
            ((List.map (\error -> li [] [ text error ]) errors))


aliasCss : Css.Snippet
aliasCss =
    (.) Alias
        [ padding2 (px 20) zero
        , children
            [ Css.input
                [ padding (px 10)
                , marginLeft (px 10)
                , Css.width (px 250)
                ]
            ]
        ]

view : Signal.Address Action -> Model -> Html
view address model =
    let
        sass =
            Sass.parse model.input
    in
        div
            [ class [ Content ] ]
            [ Util.stylesheetLink "/homepage.css"
            , viewInput address model.input
            , viewOutput sass
            , viewErrors sass
            ]

type Action
    = UpdateInput String
    | Noop

type alias Model =
    { input : String
    , errors : List String
    }

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
        Noop ->
            (model, Effects.none)
        UpdateInput input ->
            (
                { model
                    | input = input
                }
                , Effects.none)


model =
    { input = ""
    , errors = []
    }
