module Math.FinSet exposing
    ( FinSet
    , fromLabels
    , indexed
    , labelAt
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
