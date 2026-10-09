module Math.FinSet exposing
    ( FinSet
    , addElement
    , fromLabels
    , indexed
    , labelAt
    , removeLast
    , size
    )

{-| A finite set: a name (used in formulas) and an ordered list of element labels.
Elements are referred to by their index in the list.
-}


type alias FinSet =
    { name : String
    , elements : List String
    }


fromLabels : String -> List String -> FinSet
fromLabels name elements =
    { name = name, elements = elements }


{-| A set with elements named after its name: `A = {a_1, ..., a_n}`.
-}
indexed : String -> Int -> FinSet
indexed name n =
    { name = name
    , elements = List.range 1 n |> List.map (\i -> String.toLower name ++ "_" ++ String.fromInt i)
    }


size : FinSet -> Int
size =
    .elements >> List.length


labelAt : Int -> FinSet -> String
labelAt i set =
    List.drop i set.elements
        |> List.head
        |> Maybe.withDefault "?"


{-| Append an element with a label not used yet, continuing the naming pattern of the
last element where there is one (`c` ↦ `d`, `e_3` ↦ `e_4`).
-}
addElement : FinSet -> FinSet
addElement set =
    let
        n =
            List.length set.elements

        last =
            List.drop (n - 1) set.elements |> List.head |> Maybe.withDefault ""

        candidates =
            case ( String.toList last, String.split "_" last ) of
                ( [ c ], _ ) ->
                    if Char.isLower c then
                        List.range 1 26 |> List.map (\k -> String.fromChar (Char.fromCode (97 + modBy 26 (Char.toCode c - 97 + k))))

                    else
                        []

                ( _, [ prefix, digits ] ) ->
                    case String.toInt digits of
                        Just k ->
                            List.range 1 20 |> List.map (\d -> prefix ++ "_" ++ String.fromInt (k + d))

                        Nothing ->
                            []

                _ ->
                    []

        fallback =
            List.range (n + 1) (n + 40) |> List.map (\k -> "x_" ++ String.fromInt k)

        fresh =
            (candidates ++ fallback)
                |> List.filter (\l -> not (List.member l set.elements))
                |> List.head
                |> Maybe.withDefault (last ++ "'")
    in
    { set | elements = set.elements ++ [ fresh ] }


removeLast : FinSet -> FinSet
removeLast set =
    { set | elements = List.take (List.length set.elements - 1) set.elements }
