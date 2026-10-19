module Math.Setting exposing (Setting, all, allWith, byNameWith, default, examplesWith, resolveFunctors)

{-| A category (with its diagram layout) together with the Set-valued functors a reader
may pick on it, as offered by chapters 7 and 8: the curated examples of chapter 5, the
hom functors of chapter 6 and the reader's own functors from the functor playground.
-}

import ListUtil
import Math.Categories as Categories exposing (Example)
import Math.Category as Category
import Math.CustomFunctor as CustomFunctor
import Math.SetFunctor as SetFunctor exposing (SetFunctor)


type alias Setting =
    { example : Example
    , functors : List SetFunctor -- covariant F : C → Set (curated ones, then Hom(A, −), then `yours`)
    , yours : List SetFunctor -- the reader's own functors on C (also in `functors`)
    , contraFunctors : List SetFunctor -- F : C^op → Set (the hom functors Hom(−, A))
    }


{-| Every curated category, followed by the categories that only occur as the source of a
curated Set-valued functor (the "Graph shape").
-}
all : List Setting
all =
    List.map (forExample []) curatedExamples


curatedExamples : List Example
curatedExamples =
    let
        curatedOnly =
            SetFunctor.all
                |> List.filter (\f -> Categories.byName f.source.name == Nothing)
                |> List.map (.source >> Categories.layoutFor)
    in
    Categories.all ++ curatedOnly


{-| The categories of `all`, followed by the given custom categories.
-}
examplesWith : List Example -> List Example
examplesWith customs =
    curatedExamples ++ customs


{-| `all` followed by the given custom categories (which offer no curated functors), each
offering the given functors of the reader's (see `resolveFunctors`) that live on it.
-}
allWith : List Example -> List SetFunctor -> List Setting
allWith customs yours =
    List.map (forExample yours) (examplesWith customs)


byNameWith : List Example -> List SetFunctor -> String -> Maybe Setting
byNameWith customs yours name =
    ListUtil.find (\s -> s.example.category.name == name) (allWith customs yours)


{-| The saved functors that still are functors on their (curated or custom) category.
-}
resolveFunctors : List Example -> List CustomFunctor.Saved -> List SetFunctor
resolveFunctors customs saved =
    List.filterMap (CustomFunctor.resolve (examplesWith customs)) saved


forExample : List SetFunctor -> Example -> Setting
forExample yours ex =
    let
        objects =
            Category.objectIndices ex.category

        builtin =
            List.filter (\f -> f.source.name == ex.category.name) SetFunctor.all
                ++ List.map (SetFunctor.homFunctor ex.category) objects

        mine =
            -- a name already taken on this category would make the choice ambiguous
            List.filter (\f -> f.source.name == ex.category.name && not (List.any (\b -> b.name == f.name) builtin)) yours
    in
    { example = ex
    , functors = builtin ++ mine
    , yours = mine
    , contraFunctors = List.map (SetFunctor.contraHomFunctor ex.category) objects
    }


{-| The "Mixed" category, whose hom sets have several elements.
-}
default : Setting
default =
    forExample [] Categories.mixed
