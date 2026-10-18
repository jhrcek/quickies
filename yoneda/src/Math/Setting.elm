module Math.Setting exposing (Setting, all, allWith, byNameWith, default)

{-| A category (with its diagram layout) together with the Set-valued functors a reader
may pick on it, as offered by chapters 7 and 8: the curated examples of chapter 5 and the
hom functors of chapter 6.
-}

import ListUtil
import Math.Categories as Categories exposing (Example)
import Math.Category as Category
import Math.SetFunctor as SetFunctor exposing (SetFunctor)


type alias Setting =
    { example : Example
    , functors : List SetFunctor -- covariant F : C → Set (curated ones, then Hom(A, −))
    , contraFunctors : List SetFunctor -- F : C^op → Set (the hom functors Hom(−, A))
    }


{-| Every curated category, followed by the categories that only occur as the source of a
curated Set-valued functor (the "Graph shape").
-}
all : List Setting
all =
    let
        curatedOnly =
            SetFunctor.all
                |> List.filter (\f -> Categories.byName f.source.name == Nothing)
                |> List.map (.source >> Categories.layoutFor)
    in
    List.map forExample (Categories.all ++ curatedOnly)


{-| `all` followed by the given custom categories, which offer only the hom functors.
-}
allWith : List Example -> List Setting
allWith customs =
    all ++ List.map forExample customs


byNameWith : List Example -> String -> Maybe Setting
byNameWith customs name =
    ListUtil.find (\s -> s.example.category.name == name) (allWith customs)


forExample : Example -> Setting
forExample ex =
    let
        objects =
            Category.objectIndices ex.category
    in
    { example = ex
    , functors =
        List.filter (\f -> f.source.name == ex.category.name) SetFunctor.all
            ++ List.map (SetFunctor.homFunctor ex.category) objects
    , contraFunctors = List.map (SetFunctor.contraHomFunctor ex.category) objects
    }


{-| The "Mixed" category, whose hom sets have several elements.
-}
default : Setting
default =
    forExample Categories.mixed
