module Math.CustomFunctor exposing
    ( Analysis
    , Completion(..)
    , Conflict
    , Draft
    , Entry
    , Reason(..)
    , Saved
    , addElement
    , analyze
    , blank
    , choose
    , clearChoices
    , complete
    , conflictEntries
    , decoder
    , encode
    , entries
    , fromSetFunctor
    , isValid
    , maxSetSize
    , nameProblem
    , preview
    , removeElement
    , resolve
    , toDraft
    , toSaved
    , toSetFunctor
    , undecided
    )

{-| A Set-valued functor `F : C → Set` under construction, as edited in the functor
playground.

The category `C` is fixed for a draft. Every object `X` has a set `F(X)`; the function
`F(f)` of every non-identity arrow `f` is given element by element. Identities go to
identity functions. An entry `( f, x )` stands for the value `F(f)(x)`. The user's
choices are stored; further values are _forced_, either because the target set has a
single element or by the composition law: for every composable pair of non-identity
arrows with `f ; g = h` and every `x ∈ F(source of f)`,

    F(h)(x) = F(g)(F(f)(x))

so knowing `F(f)(x)` and one side determines the other side.

Composition is diagrammatic: `f ; g` is "f then g".

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListUtil
import Math.Categories exposing (Example)
import Math.Category as Category exposing (Category)
import Math.FinFunction as FinFunction
import Math.FinSet as FinSet exposing (FinSet)
import Math.SetFunctor as SetFunctor exposing (SetFunctor)
import Set


type alias Draft =
    { name : String
    , example : Example -- the source category with its layout
    , sets : Array FinSet -- F(X), by object index
    , choices : Dict Entry Int -- ( non-identity arrow f, element x of F(source of f) ) ↦ F(f)(x)
    }


{-| `( f, x )`: the value of `F(f)` at the element `x` of `F(source of f)`.
-}
type alias Entry =
    ( Int, Int )


{-| Why a value is known.
-}
type Reason
    = Chosen
    | OnlyElement -- the target set has a single element
    | ByLaw Int Int Int -- f, g, x: the composition law of f ; g at x


{-| An instance of the composition law that fails: `F(h)(x) = left`, but
`F(g)(F(f)(x)) = right`, where `h = f ; g`.
-}
type alias Conflict =
    { f : Int
    , g : Int
    , h : Int
    , x : Int
    , left : Int
    , right : Int
    }


type alias Analysis =
    { values : Dict Entry ( Int, Reason )
    , conflicts : List Conflict
    }


maxSetSize : Int
maxSetSize =
    8


{-| Every set `F(X)` with `n` elements and nothing chosen yet.
-}
blank : Int -> Example -> Draft
blank n ex =
    { name = "My functor"
    , example = ex
    , sets = Array.fromList (List.map (\o -> defaultSet ex.category o n) (Category.objectIndices ex.category))
    , choices = Dict.empty
    }


{-| `F(A) = {a_1, …, a_n}` when `A` is a single letter, `{x_1, …, x_n}` otherwise.
-}
defaultSet : Category -> Int -> Int -> FinSet
defaultSet cat o n =
    let
        lbl =
            Category.objectLabel cat o

        base =
            case String.toList lbl of
                [ c ] ->
                    if Char.isAlpha c then
                        String.toLower lbl

                    else
                        "x"

                _ ->
                    "x"
    in
    FinSet.fromLabels (setName cat o) (List.map (\i -> base ++ "_" ++ String.fromInt i) (List.range 1 n))


setName : Category -> Int -> String
setName cat o =
    "F(" ++ Category.objectLabel cat o ++ ")"


texName : Draft -> String
texName d =
    "\\text{" ++ String.trim d.name ++ "} : " ++ d.example.category.texName ++ " \\to \\mathbf{Set}"



-- QUERIES


category : Draft -> Category
category d =
    d.example.category


setAt : Draft -> Int -> FinSet
setAt d o =
    Array.get o d.sets |> Maybe.withDefault (FinSet.fromLabels "?" [])


sizeAt : Draft -> Int -> Int
sizeAt d o =
    FinSet.size (setAt d o)


nonIdentities : Category -> List Int
nonIdentities cat =
    Category.morphismIndices cat |> List.filter (not << Category.isIdentity cat)


ends : Category -> Int -> ( Int, Int )
ends cat f =
    Category.morphism cat f |> Maybe.map (\m -> ( m.src, m.tgt )) |> Maybe.withDefault ( 0, 0 )


{-| All entries, arrow by arrow, in morphism-index order.
-}
entries : Draft -> List Entry
entries d =
    nonIdentities (category d)
        |> List.concatMap (\f -> List.range 0 (sizeAt d (Tuple.first (ends (category d) f)) - 1) |> List.map (Tuple.pair f))


{-| Entries whose value is not known.
-}
undecided : Draft -> Analysis -> List Entry
undecided d analysis =
    entries d |> List.filter (\e -> not (Dict.member e analysis.values))


{-| Every value is known and the composition law holds everywhere.
-}
isValid : Draft -> Bool
isValid d =
    toSetFunctor d /= Nothing


{-| The entries taking part in a failing instance of the composition law.
-}
conflictEntries : Draft -> Analysis -> List Entry
conflictEntries d analysis =
    let
        cat =
            category d

        valueAt f x =
            Dict.get ( f, x ) analysis.values |> Maybe.map Tuple.first
    in
    analysis.conflicts
        |> List.concatMap
            (\c ->
                ( c.f, c.x )
                    :: List.filterMap identity
                        [ valueAt c.f c.x |> Maybe.map (Tuple.pair c.g)
                        , if Category.isIdentity cat c.h then
                            Nothing

                          else
                            Just ( c.h, c.x )
                        ]
            )
        |> List.foldl
            (\e acc ->
                if List.member e acc then
                    acc

                else
                    acc ++ [ e ]
            )
            []



-- PROPAGATION


{-| What propagation needs at hand: the set sizes, and the instances of the composition
law each arrow takes part in, by its role in `f ; g = h`.
-}
type alias Ctx =
    { cat : Category
    , sizes : Array Int
    , asFirst : Dict Int (List ( Int, Int )) -- f ↦ ( g, h )
    , asSecond : Dict Int (List ( Int, Int )) -- g ↦ ( f, h )
    , asComposite : Dict Int (List ( Int, Int )) -- h ↦ ( f, g )
    }


context : Draft -> Ctx
context d =
    let
        cat =
            category d

        add key v =
            Dict.update key (\old -> Just (v :: Maybe.withDefault [] old))

        laws =
            Category.composablePairs cat
                |> List.filter (\( f, g ) -> not (Category.isIdentity cat f) && not (Category.isIdentity cat g))
                |> List.filterMap (\( f, g ) -> Category.compose cat f g |> Maybe.map (\h -> ( f, g, h )))
    in
    List.foldr
        (\( f, g, h ) ctx ->
            { ctx
                | asFirst = add f ( g, h ) ctx.asFirst
                , asSecond = add g ( f, h ) ctx.asSecond
                , asComposite = add h ( f, g ) ctx.asComposite
            }
        )
        { cat = cat
        , sizes = Array.map FinSet.size d.sets
        , asFirst = Dict.empty
        , asSecond = Dict.empty
        , asComposite = Dict.empty
        }
        laws


{-| `F(h)(x) = F(g)(F(f)(x))` for `f ; g = h`.
-}
type alias Instance =
    { f : Int, g : Int, h : Int, x : Int }


type alias State =
    { values : Dict Entry ( Int, Reason )
    , conflicts : List Conflict
    , work : Int -- instances of the law looked at
    }


valueOf : Ctx -> Dict Entry ( Int, Reason ) -> Int -> Int -> Maybe Int
valueOf ctx values f x =
    if Category.isIdentity ctx.cat f then
        Just x

    else
        Dict.get ( f, x ) values |> Maybe.map Tuple.first


sourceSize : Ctx -> Int -> Int
sourceSize ctx f =
    Array.get (Tuple.first (ends ctx.cat f)) ctx.sizes |> Maybe.withDefault 0


{-| Derive everything the law forces from the entries in `todo` (whose values are known).
With `stopEarly`, stop at the first conflict.
-}
propagate : Ctx -> Bool -> List Entry -> State -> State
propagate ctx stopEarly todo state =
    case todo of
        [] ->
            state

        ( k, x ) :: rest ->
            if stopEarly && not (List.isEmpty state.conflicts) then
                state

            else
                let
                    instances =
                        List.map (\( g, h ) -> { f = k, g = g, h = h, x = x }) (Maybe.withDefault [] (Dict.get k ctx.asFirst))
                            ++ List.concatMap
                                (\( f, h ) ->
                                    List.range 0 (sourceSize ctx f - 1)
                                        |> List.filter (\x2 -> valueOf ctx state.values f x2 == Just x)
                                        |> List.map (\x2 -> { f = f, g = k, h = h, x = x2 })
                                )
                                (Maybe.withDefault [] (Dict.get k ctx.asSecond))
                            ++ List.map (\( f, g ) -> { f = f, g = g, h = k, x = x }) (Maybe.withDefault [] (Dict.get k ctx.asComposite))

                    ( next, more ) =
                        List.foldl (checkInstance ctx) ( state, [] ) instances
                in
                propagate ctx stopEarly (more ++ rest) next


{-| Look at `F(h)(x) = F(g)(F(f)(x))`: report a conflict if both sides are known and
differ, or set the unknown side if the other one is known.
-}
checkInstance : Ctx -> Instance -> ( State, List Entry ) -> ( State, List Entry )
checkInstance ctx { f, g, h, x } ( state, found ) =
    let
        counted =
            { state | work = state.work + 1 }
    in
    case valueOf ctx state.values f x of
        Nothing ->
            ( counted, found )

        Just a ->
            case ( valueOf ctx state.values g a, valueOf ctx state.values h x ) of
                ( Just right, Just left ) ->
                    if left == right then
                        ( counted, found )

                    else
                        let
                            conflict =
                                { f = f, g = g, h = h, x = x, left = left, right = right }
                        in
                        if List.member conflict counted.conflicts then
                            ( counted, found )

                        else
                            ( { counted | conflicts = counted.conflicts ++ [ conflict ] }, found )

                ( Just right, Nothing ) ->
                    ( { counted | values = Dict.insert ( h, x ) ( right, ByLaw f g x ) counted.values }, ( h, x ) :: found )

                ( Nothing, Just left ) ->
                    ( { counted | values = Dict.insert ( g, a ) ( left, ByLaw f g x ) counted.values }, ( g, a ) :: found )

                ( Nothing, Nothing ) ->
                    ( counted, found )


{-| The values known from the choices (those still in range), the one-element target sets
and the composition law, and the instances of the law that fail.
-}
analyze : Draft -> Analysis
analyze d =
    let
        state =
            run (context d) False d d.choices
    in
    { values = state.values, conflicts = state.conflicts }


{-| Seed the choices and the entries into one-element sets, then propagate.
-}
run : Ctx -> Bool -> Draft -> Dict Entry Int -> State
run ctx stopEarly d choices =
    let
        cat =
            category d

        targetSize f =
            Array.get (Tuple.second (ends cat f)) ctx.sizes |> Maybe.withDefault 0

        chosen =
            choices
                |> Dict.filter (\( f, x ) y -> x < sourceSize ctx f && 0 <= y && y < targetSize f)
                |> Dict.map (\_ y -> ( y, Chosen ))

        values =
            entries d
                |> List.foldl
                    (\(( f, _ ) as e) acc ->
                        if not (Dict.member e acc) && targetSize f == 1 then
                            Dict.insert e ( 0, OnlyElement ) acc

                        else
                            acc
                    )
                    chosen
    in
    propagate ctx stopEarly (Dict.keys values) { values = values, conflicts = [], work = 0 }



-- EDITING


{-| Choose `F(f)(x) = y`; `Nothing` clears the choice.
-}
choose : Entry -> Maybe Int -> Draft -> Draft
choose e y d =
    case y of
        Just v ->
            { d | choices = Dict.insert e v d.choices }

        Nothing ->
            { d | choices = Dict.remove e d.choices }


clearChoices : Draft -> Draft
clearChoices d =
    { d | choices = Dict.empty }


{-| Add an element to `F(X)`. Values that were forced because `F(X)` had a single element
are recorded as choices first, so the new element does not unsettle them.
-}
addElement : Int -> Draft -> Draft
addElement o d =
    if sizeAt d o >= maxSetSize then
        d

    else
        let
            values =
                (analyze d).values

            pinned =
                Dict.foldl
                    (\( f, x ) ( y, reason ) acc ->
                        if reason == OnlyElement && Tuple.second (ends (category d) f) == o then
                            Dict.insert ( f, x ) y acc

                        else
                            acc
                    )
                    d.choices
                    values
        in
        { d | sets = Array.set o (FinSet.addElement (setAt d o)) d.sets, choices = pinned }


{-| Remove the last element of `F(X)`, and every choice mentioning it.
-}
removeElement : Int -> Draft -> Draft
removeElement o d =
    let
        n =
            sizeAt d o
    in
    if n <= 0 then
        d

    else
        let
            cat =
                category d

            mentions ( f, x ) y =
                let
                    ( src, tgt ) =
                        ends cat f
                in
                (src == o && x == n - 1) || (tgt == o && y == n - 1)
        in
        { d
            | sets = Array.set o (FinSet.removeLast (setAt d o)) d.sets
            , choices = Dict.filter (\e y -> not (mentions e y)) d.choices
        }



-- AUTOMATIC COMPLETION


type Completion
    = Completed Draft
    | NoLawfulCompletion
    | GaveUp


{-| How many instances of the composition law the search may look at before giving up.
-}
searchBudget : Int
searchBudget =
    3000000


{-| Fill in the undecided values so that the composition law holds, keeping the choices
made so far: a depth-first search that propagates every guess and backtracks as soon as
the law fails. The guesses are added to the choices; what they force stays forced.

Arrows that are not composites of two other non-identity arrows are guessed first: the
values of the others then mostly follow.

-}
complete : Draft -> Completion
complete d =
    let
        ctx =
            context d

        start =
            run ctx True d d.choices
    in
    if not (List.isEmpty start.conflicts) then
        NoLawfulCompletion

    else
        let
            ( generators, composites ) =
                List.partition (\( f, _ ) -> not (Dict.member f ctx.asComposite)) (entries d)

            order =
                generators ++ composites
        in
        case search ctx order start.values d.choices searchBudget of
            ( _, Just choices ) ->
                Completed { d | choices = choices }

            ( budget, Nothing ) ->
                if budget <= 0 then
                    GaveUp

                else
                    NoLawfulCompletion


search : Ctx -> List Entry -> Dict Entry ( Int, Reason ) -> Dict Entry Int -> Int -> ( Int, Maybe (Dict Entry Int) )
search ctx order values choices budget =
    case ListUtil.find (\e -> not (Dict.member e values)) order of
        Nothing ->
            ( budget, Just choices )

        Just (( f, _ ) as e) ->
            let
                tgtSize =
                    Array.get (Tuple.second (ends ctx.cat f)) ctx.sizes |> Maybe.withDefault 0
            in
            tryEach ctx order values choices budget e (List.range 0 (tgtSize - 1))


tryEach : Ctx -> List Entry -> Dict Entry ( Int, Reason ) -> Dict Entry Int -> Int -> Entry -> List Int -> ( Int, Maybe (Dict Entry Int) )
tryEach ctx order values choices budget e options =
    case options of
        [] ->
            ( budget, Nothing )

        y :: more ->
            if budget <= 0 then
                ( 0, Nothing )

            else
                let
                    state =
                        propagate ctx True [ e ] { values = Dict.insert e ( y, Chosen ) values, conflicts = [], work = 0 }

                    left =
                        budget - state.work - 1
                in
                if List.isEmpty state.conflicts then
                    case search ctx order state.values (Dict.insert e y choices) left of
                        ( remaining, Just done ) ->
                            ( remaining, Just done )

                        ( remaining, Nothing ) ->
                            tryEach ctx order values choices remaining e more

                else
                    tryEach ctx order values choices left e more



-- CONVERSION


{-| The draft as a `SetFunctor`, with `-1` for the values not known yet (the set picture
leaves those out). Good enough to draw it.
-}
preview : Draft -> Analysis -> SetFunctor
preview d analysis =
    let
        cat =
            category d

        fun f =
            let
                ( src, tgt ) =
                    ends cat f
            in
            if Category.isIdentity cat f then
                FinFunction.identity (setAt d src)

            else
                FinFunction.fromList (setAt d src)
                    (setAt d tgt)
                    (List.range 0 (sizeAt d src - 1)
                        |> List.map (\x -> Dict.get ( f, x ) analysis.values |> Maybe.map Tuple.first |> Maybe.withDefault -1)
                    )
    in
    { name = String.trim d.name
    , texName = texName d
    , description = "A Set-valued functor of your own, built in the functor playground and saved in this browser."
    , source = cat
    , objects = d.sets
    , morphisms = Array.fromList (List.map fun (Category.morphismIndices cat))
    }


{-| The finished functor, if the draft is one.
-}
toSetFunctor : Draft -> Maybe SetFunctor
toSetFunctor d =
    let
        analysis =
            analyze d
    in
    if List.isEmpty analysis.conflicts && List.isEmpty (undecided d analysis) then
        let
            fun =
                preview d analysis
        in
        if SetFunctor.isFunctor fun then
            Just fun

        else
            Nothing

    else
        Nothing


{-| Any Set-valued functor on the given category as a draft. Only the values that are not
forced by earlier ones become choices, so that the forced ones show as such.
-}
fromSetFunctor : Example -> SetFunctor -> Draft
fromSetFunctor ex fun =
    let
        cat =
            ex.category

        sets =
            Category.objectIndices cat
                |> List.map
                    (\o ->
                        let
                            set =
                                SetFunctor.objectImage fun o
                        in
                        { set | name = setName cat o }
                    )
                |> Array.fromList

        start =
            { name = fun.name, example = ex, sets = sets, choices = Dict.empty }
    in
    minimize start
        (\( f, x ) ->
            let
                y =
                    FinFunction.apply (SetFunctor.morphismImage fun f) x
            in
            if 0 <= y && y < sizeAt start (Tuple.second (ends cat f)) then
                Just y

            else
                Nothing
        )


{-| Go through the entries, generators first, and choose the given value of each entry
that is not known by then.
-}
minimize : Draft -> (Entry -> Maybe Int) -> Draft
minimize d valueAt =
    let
        ctx =
            context d

        ( generators, composites ) =
            List.partition (\( f, _ ) -> not (Dict.member f ctx.asComposite)) (entries d)

        step e ( values, choices ) =
            if Dict.member e values then
                ( values, choices )

            else
                case valueAt e of
                    Just y ->
                        ( (propagate ctx False [ e ] { values = Dict.insert e ( y, Chosen ) values, conflicts = [], work = 0 }).values
                        , Dict.insert e y choices
                        )

                    Nothing ->
                        ( values, choices )

        ( _, minimal ) =
            List.foldl step ( (run ctx False d Dict.empty).values, Dict.empty ) (generators ++ composites)
    in
    { d | choices = minimal }



-- PERSISTENCE


{-| A functor as stored, referring to its category by name and to objects and arrows by
label, so that it survives (or visibly breaks under) edits of a custom category.
-}
type alias Saved =
    { name : String
    , category : String
    , sets : List ( String, List String ) -- object label, element labels
    , functions : List ( String, List Int ) -- arrow label, images of the elements (-1: unknown)
    }


{-| Every known value is written out, forced ones included.
-}
toSaved : Draft -> Saved
toSaved d =
    let
        cat =
            category d

        analysis =
            analyze d
    in
    { name = String.trim d.name
    , category = cat.name
    , sets = Category.objectIndices cat |> List.map (\o -> ( Category.objectLabel cat o, (setAt d o).elements ))
    , functions =
        nonIdentities cat
            |> List.map
                (\f ->
                    ( Category.morphismLabel cat f
                    , List.range 0 (sizeAt d (Tuple.first (ends cat f)) - 1)
                        |> List.map (\x -> Dict.get ( f, x ) analysis.values |> Maybe.map Tuple.first |> Maybe.withDefault -1)
                    )
                )
    }


{-| A stored functor on the given category, keeping what still fits: the sets of objects
with the same label, the functions of arrows with the same label and ends.
-}
toDraft : Example -> Saved -> Draft
toDraft ex saved =
    let
        cat =
            ex.category

        olbl =
            Category.objectLabel cat

        sets =
            Category.objectIndices cat
                |> List.map
                    (\o ->
                        case ListUtil.find (\( l, _ ) -> l == olbl o) saved.sets of
                            Just ( _, elements ) ->
                                if List.length elements > maxSetSize || not (ListUtil.allDistinct elements) then
                                    defaultSet cat o 1

                                else
                                    FinSet.fromLabels (setName cat o) elements

                            Nothing ->
                                defaultSet cat o 1
                    )
                |> Array.fromList

        start =
            { name = saved.name, example = ex, sets = sets, choices = Dict.empty }

        stored =
            nonIdentities cat
                |> List.filterMap
                    (\f ->
                        ListUtil.find (\( l, _ ) -> l == Category.morphismLabel cat f) saved.functions
                            |> Maybe.map (\( _, images ) -> ( f, Array.fromList images ))
                    )
                |> Dict.fromList

        valueAt ( f, x ) =
            Dict.get f stored
                |> Maybe.andThen (Array.get x)
                |> Maybe.andThen
                    (\y ->
                        let
                            ( _, tgt ) =
                                ends cat f
                        in
                        if 0 <= y && y < sizeAt start tgt then
                            Just y

                        else
                            Nothing
                    )
    in
    minimize start valueAt


{-| The stored functor, if its category exists and it is still a functor on it.
-}
resolve : List Example -> Saved -> Maybe SetFunctor
resolve examples saved =
    ListUtil.find (\ex -> ex.category.name == saved.category) examples
        |> Maybe.andThen (\ex -> toSetFunctor (toDraft ex saved))


{-| A name is required, must not contain TeX special characters (it is shown as
`\text{…}`), must not be one of `taken` and must not look like the name of a hom functor.
-}
nameProblem : List String -> String -> Maybe String
nameProblem taken name =
    let
        trimmed =
            String.trim name
    in
    if trimmed == "" then
        Just "Give the functor a name."

    else if String.length trimmed > 40 then
        Just "The name is too long (at most 40 characters)."

    else if String.any (\ch -> String.contains (String.fromChar ch) "\\{}$&#^_%~") trimmed then
        Just "The name may not contain any of \\ { } $ & # ^ _ % ~"

    else if List.member trimmed taken then
        Just "A built-in functor already has this name."

    else if String.startsWith "Hom(" trimmed then
        Just "Names starting with “Hom(” are kept for the hom functors."

    else
        Nothing


encode : List Saved -> Encode.Value
encode saved =
    Encode.object
        [ ( "version", Encode.int 1 )
        , ( "functors", Encode.list encodeSaved saved )
        ]


encodeSaved : Saved -> Encode.Value
encodeSaved s =
    Encode.object
        [ ( "name", Encode.string s.name )
        , ( "category", Encode.string s.category )
        , ( "sets"
          , Encode.list (\( o, els ) -> Encode.object [ ( "object", Encode.string o ), ( "elements", Encode.list Encode.string els ) ]) s.sets
          )
        , ( "functions"
          , Encode.list (\( f, images ) -> Encode.object [ ( "arrow", Encode.string f ), ( "images", Encode.list Encode.int images ) ]) s.functions
          )
        ]


{-| Stored functors with distinct names (later duplicates are dropped).
-}
decoder : Decoder (List Saved)
decoder =
    Decode.field "functors"
        (Decode.list
            (Decode.map4 Saved
                (Decode.field "name" Decode.string)
                (Decode.field "category" Decode.string)
                (Decode.field "sets"
                    (Decode.list
                        (Decode.map2 Tuple.pair
                            (Decode.field "object" Decode.string)
                            (Decode.field "elements" (Decode.list Decode.string))
                        )
                    )
                )
                (Decode.field "functions"
                    (Decode.list
                        (Decode.map2 Tuple.pair
                            (Decode.field "arrow" Decode.string)
                            (Decode.field "images" (Decode.list Decode.int))
                        )
                    )
                )
            )
            |> Decode.map
                (List.foldl
                    (\s ( seen, acc ) ->
                        if Set.member s.name seen then
                            ( seen, acc )

                        else
                            ( Set.insert s.name seen, acc ++ [ s ] )
                    )
                    ( Set.empty, [] )
                    >> Tuple.second
                )
        )
