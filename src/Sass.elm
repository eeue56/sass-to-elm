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

capitalize : String -> String
capitalize name =
    case String.toList name of
        [] ->
            ""
        x::xs ->
            (String.toUpper (String.fromChar x)) ++ (String.fromList xs)

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
                            List.map createField (List.map String.trim xs)
                                |> List.filter ((/=) Nothing)
                                |> List.map (Maybe.withDefault emptyField)
                        }
                else
                    Nothing

parse : String -> String
parse values =
    findClasses values
        |> List.map (\(name, rest) -> String.join "\n" (name :: rest ))
        |> List.map createClass
        |> List.map (Maybe.map classFormat)
        |> List.map (Maybe.withDefault "")
        |> String.join "\n\n"


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []

findClasses : String -> List (String, List String)
findClasses text =
    let
        lines =
            String.lines text
                |> List.filter (String.trim >> ((/=) ""))

        untilNextClass =
            takeWhile (not << String.startsWith ".")

        _ =
            Debug.log "here" lines
    in
        case lines of
            [] ->
                []
            class::rest ->
                if String.startsWith "." class then
                    Debug.log "here2" <| (class, untilNextClass rest) :: findClasses (String.join "\n" rest)
                else
                    findClasses (String.join "\n" rest)


