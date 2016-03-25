module Sass where

import String
import Regex

type FieldType
    = Unit String String
    | Unknown

type alias Field =
    { name : String
    , value : String
    , fieldType : FieldType
    }

emptyField =
    { name = ""
    , value = ""
    , fieldType = Unknown
    }

type alias Class =
    { name : String
    , fields : List Field
    }

unitFormat : String -> String -> String
unitFormat unit value =
    "(" ++ unit ++ " " ++ value ++ ")"

splitUnit : String -> (String, String)
splitUnit value =
    if String.endsWith "px" value then
        (String.dropRight 2 value, "px")
    else
        (value, "")


fieldValueFormat : FieldType -> String
fieldValueFormat field =
    case field of
        Unknown ->
            ""
        Unit value unit ->
            unitFormat unit value


fieldFormat : Field -> String
fieldFormat field =
    field.name ++ (fieldValueFormat field.fieldType)

guessFieldType : String -> FieldType
guessFieldType value =
    if String.contains "px" value then
        let
            (unitValue, unit) =
                splitUnit value
        in
            Unit unitValue unit
    else
        Unknown

createField : String -> Maybe Field
createField text =
    case String.split ":" text of
        x::y::_ ->
            Just
                { name = x
                , value = y
                , fieldType = guessFieldType y
                }

        _ ->
            Nothing


classFormat : Class -> String
classFormat class =
    let
        formattedFields =
            List.map fieldFormat class.fields
    in
        String.join ""
            [ "(.) "
            , capitalize class.name
            , "\n    [ "
            , String.join "\n    , " formattedFields
            , "\n    ]"
            ]

createClass : String -> Maybe Class
createClass text =
    let
        lines =
            String.lines text
    in
        case lines of
            [] ->
                Nothing
            x::xs ->
                if String.startsWith "." x then
                    Just
                        { name = String.dropLeft 1 x
                        , fields =
                            List.map createField (Debug.log "hu" (List.map String.trim xs))
                                |> List.filter ((/=) Nothing)
                                |> List.map (Maybe.withDefault emptyField)
                        }
                else
                    Nothing

parse : String -> String
parse values =
    createClass values
        |> Maybe.map classFormat
        |> Maybe.withDefault ""


capitalize : String -> String
capitalize name =
    case String.toList name of
        [] ->
            ""
        x::xs ->
            (String.toUpper (String.fromChar x)) ++ (String.fromList xs)
