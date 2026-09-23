module Query exposing
    ( Query
    , fromString
    , int
    , intList
    , intListParam
    , intLists
    , intListsParam
    , param
    , string
    , toString
    )

{-| Deep-link state of a chapter, stored after the `?` inside the URL hash:
`#/categories?c=Mixed&f=2&g=3`. Keys and values are percent-encoded.

Pages expose `toQuery : Model -> Query` and `fromQuery : Query -> Model -> Model`; the
latter applies every parameter it recognises and ignores the rest, so partial or stale
links still load.

-}

import Dict exposing (Dict)
import Url


type alias Query =
    Dict String String


{-| Build a parameter for `toQuery`.
-}
param : String -> String -> ( String, String )
param key value =
    ( key, value )


{-| A parameter holding comma-separated integers, read back by `intList`.
-}
intListParam : String -> List Int -> ( String, String )
intListParam key xs =
    ( key, encodeIntList xs )


{-| A parameter holding semicolon-separated integer lists, read back by `intLists`.
-}
intListsParam : String -> List (List Int) -> ( String, String )
intListsParam key xss =
    ( key, String.join ";" (List.map encodeIntList xss) )


encodeIntList : List Int -> String
encodeIntList =
    List.map String.fromInt >> String.join ","


string : String -> Query -> Maybe String
string key q =
    Dict.get key q


int : String -> Query -> Maybe Int
int key q =
    Dict.get key q |> Maybe.andThen String.toInt


{-| Comma-separated integers, e.g. `0,2,1`. An empty string is the empty list.
-}
intList : String -> Query -> Maybe (List Int)
intList key q =
    Dict.get key q |> Maybe.andThen parseIntList


{-| Semicolon-separated lists of comma-separated integers, e.g. `0,1;2`.
-}
intLists : String -> Query -> Maybe (List (List Int))
intLists key q =
    Dict.get key q
        |> Maybe.andThen
            (\s ->
                String.split ";" s
                    |> List.map parseIntList
                    |> List.foldr (Maybe.map2 (::)) (Just [])
            )


parseIntList : String -> Maybe (List Int)
parseIntList s =
    if s == "" then
        Just []

    else
        String.split "," s
            |> List.map String.toInt
            |> List.foldr (Maybe.map2 (::)) (Just [])


{-| Parse the part of a hash after `?`.
-}
fromString : String -> Query
fromString s =
    if s == "" then
        Dict.empty

    else
        String.split "&" s
            |> List.filterMap
                (\pair ->
                    case String.split "=" pair of
                        key :: rest ->
                            Maybe.map2 Tuple.pair
                                (Url.percentDecode key)
                                (Url.percentDecode (String.join "=" rest))

                        [] ->
                            Nothing
                )
            |> Dict.fromList


{-| Render as `key=value&...` (no leading `?`); the empty query renders as `""`.
-}
toString : List ( String, String ) -> String
toString params =
    params
        |> List.map (\( k, v ) -> Url.percentEncode k ++ "=" ++ Url.percentEncode v)
        |> String.join "&"
