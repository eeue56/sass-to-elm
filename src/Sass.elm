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
    , indent : Int
    }

emptyField =
    { name = ""
    , value = ""
    , fieldType = Unknown
    , indent = 0
    }

type Symbol
    = Class
        { name : String
        , fields : List Symbol
        , indent : Int
        }
    | Id
        { name : String
        , fields : List Symbol
        , indent : Int
        }
    | Rule Field

type UnparsedSymbol
    = UnparsedClass
        { name : String
        , fields : List (Int, String)
        , indent : Int
        }
    | UnparsedId
        { name : String
        , fields : List (Int, String)
        , indent : Int
        }
    | UnparsedRule
        { line : String
        , indent : Int
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
    else if String.endsWith "%" value then
        (String.dropRight 1 value, "pct")
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
    (String.trim field.name) ++ " " ++ (fieldValueFormat field.fieldType)

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
                , indent = 0
                }

        _ ->
            Nothing


symbolFormat : Symbol -> String
symbolFormat symbol =
    let
        classFormat class =
            let
                (classes, rest) =
                    List.partition
                        (\x ->
                            case x of
                                Class _ ->
                                    True
                                _ ->
                                    False
                        ) class.fields

                formattedFields =
                    List.map symbolFormat rest

                formattedClasses =
                    List.map symbolFormat classes
                        |> List.map (\x -> "   " ++ x)

                firstIndent =
                    List.repeat class.indent " "
                        |> String.join ""

                afterIndent =
                    List.repeat (class.indent + 4) " "
                        |> String.join ""

                indenter line =
                    afterIndent ++ line


                comma =
                    "\n" ++ (indenter ", ")

            in
                String.join ""
                    [ firstIndent ++ ("(.) " ++ (capitalize class.name))
                    , "\n"
                    , indenter "[ "
                    , ( String.join comma <| (formattedFields ++ formattedClasses))
                    , "\n"
                    , indenter "]"
                    ]

    in
        case symbol of
            Class class ->
                classFormat class
            Id class ->
                classFormat class
            Rule rule ->
                fieldFormat rule


isClassSymbol : String -> Bool
isClassSymbol symbol =
    String.trim symbol
        |> String.startsWith "."

isFieldSymbol : String -> Bool
isFieldSymbol symbol =
    String.trim symbol
        |> String.contains ":"

createSymbol : UnparsedSymbol -> Maybe Symbol
createSymbol symbol =
    case symbol of
        UnparsedRule rule ->
            createField rule.line
                |> Maybe.map Rule

        UnparsedClass class ->
            Class
                { name =
                    class.name

                , fields =
                    findSymbols class.fields
                        |> List.filterMap createSymbol
                , indent =
                    class.indent
                }
                |> Just

        UnparsedId id ->
            Nothing

parse : String -> String
parse values =
    findSymbolsFromText values
        |> List.map createSymbol
        |> List.map (Maybe.map symbolFormat)
        |> List.map (Maybe.withDefault "")
        |> String.join "\n\n"


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []

findSymbolsFromText : String -> List UnparsedSymbol
findSymbolsFromText =
    linesWithIndent
        >> List.filter (snd >> String.trim >> ((/=) ""))
        >> findSymbols

findSymbols : List (Int, String) -> List UnparsedSymbol
findSymbols lines =
    let
        untilNextClass indent rest =
            takeWhile (fst >> (\x -> x > indent)) rest

    in
        case lines of
            [] ->
                []
            (indent, line)::rest ->
                let
                    tilNextClass =
                        untilNextClass indent rest

                    leftOvers =
                        List.drop (List.length tilNextClass) rest
                in
                    if isClassSymbol line then
                        UnparsedClass
                            { name = String.dropLeft 1 (String.trim line)
                            , fields = tilNextClass
                            , indent = indent
                            } :: findSymbols leftOvers
                    else if isFieldSymbol line then
                        UnparsedRule
                            { line = line
                            , indent = indent
                            } :: findSymbols leftOvers
                    else
                        findSymbols rest


removeIndent : Int -> String -> String
removeIndent amount text =
    let
        indent =
            List.repeat amount " "
                |> String.join ""
    in
        String.lines text
            |> List.map (\line ->
                if String.startsWith indent line then
                    String.dropLeft amount line
                else
                    line
                    )
            |> String.join "\n"


countChar : String -> String -> Int
countChar letter text =
    if text == "" then
        0
    else if String.startsWith letter text then
        1 + (countChar letter (String.dropLeft 1 text))
    else
        countChar letter (String.dropLeft 1 text)

hasConsistentIndentation : List Int -> Bool
hasConsistentIndentation indentLevels =
    let
        smallest : Int
        smallest =
            List.filter ((==) 0) indentLevels
                |> List.minimum
                |> Maybe.withDefault 0

        isDivisible : Int -> Int -> Bool
        isDivisible a b =
            a % b == 0

    in
        List.all (isDivisible smallest) indentLevels

linesWithIndent : String -> List (Int, String)
linesWithIndent text =
    let
        lines =
            String.lines text
    in
        lines
            |> List.map (\line -> (countChar " " line, line))
