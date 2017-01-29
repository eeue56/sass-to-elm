module CssValidator exposing (..)

import Css
import Matrix
import Array
import String
import Native.CssValidator


isValidElmCssFunction : String -> Bool
isValidElmCssFunction name =
    Native.CssValidator.isValid name


names : List String
names =
    Native.CssValidator.names


numberOfArgs : String -> Int
numberOfArgs name =
    Native.CssValidator.numberOfArgs name


suggestions : List String -> String -> List String
suggestions names input =
    List.map (\x -> ( levDistance input x, x )) names
        |> List.filter (\x -> (Tuple.first x) < 5)
        |> List.sortBy (Tuple.first)
        |> List.take 5
        |> List.map Tuple.second


levDistance : String -> String -> Int
levDistance =
    Native.CssValidator.levDistance


{-|
  returns levenshtein distance between two strings
-}
levenshtein : String -> String -> Int
levenshtein s1_ s2_ =
    let
        unsafeGet : Int -> Int -> Matrix.Matrix Int -> Int
        unsafeGet i j m =
            Maybe.withDefault 999 (Matrix.get i j m)

        unsafeConcatV r m =
            Maybe.withDefault m (Matrix.concatVertical r m)

        unsafeConcatH c m =
            Maybe.withDefault m (Matrix.concatHorizontal c m)

        unsafeFromList xs =
            Maybe.withDefault Matrix.empty (Matrix.fromList xs)

        s1 : Array.Array Char
        s1 =
            Array.fromList <| String.toList s1_

        s2 : Array.Array Char
        s2 =
            Array.fromList <| String.toList s2_

        l1 =
            Array.length s1

        l2 =
            Array.length s2

        cost i j =
            if Array.get (i - 1) s1 == Array.get (j - 1) s2 then
                0
            else
                1

        levInversion i j m =
            if
                i
                    > 1
                    && j
                    > 1
                    && Array.get (i - 1) s1
                    == Array.get (j - 2) s2
                    && Array.get (j - 2) s1
                    == Array.get (j - 1) s2
            then
                min (levStep i j m) ((unsafeGet (i - 2) (j - 2) m) + 1)
            else
                levStep i j m

        levStep : Int -> Int -> Matrix.Matrix Int -> Int
        levStep i j m =
            case
                List.minimum
                    [ unsafeGet (i - 1) j m + 1
                    , unsafeGet i (j - 1) m + 1
                    , unsafeGet (i - 1) (j - 1) m + (cost i j)
                    ]
            of
                Just v ->
                    v

                Nothing ->
                    0

        init : Matrix.Matrix Int
        init =
            unsafeConcatH
                (unsafeFromList <| List.map (\x -> [ x ]) (List.range 0 l2))
            <|
                unsafeConcatV
                    (unsafeFromList [ List.range 1 l1 ])
                    (Matrix.repeat l1 l2 0)

        step : Int -> Matrix.Matrix Int -> Matrix.Matrix Int
        step i acc =
            List.foldl (\j m -> Matrix.set i j (levInversion i j m) m) acc (List.range 1 l2)
    in
        unsafeGet l1 l2 (List.foldl step init (List.range 1 l1))
