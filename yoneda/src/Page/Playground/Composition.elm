module Page.Playground.Composition exposing (Model, Msg, fromQuery, init, toQuery, update, view, withSeed)

{-| Composition in Set playground: three finite sets `A`, `B`, `C` in a triangle with
functions `f : A → B`, `g : B → C` and `h : A → C`. Three modes say which function is the
unknown one:

  - composition: given `f` and `g`, find `h` with `f ; g = h`;
  - extension: given `f` and `h`, find `g` with `f ; g = h`;
  - lifting: given `g` and `h`, find `f` with `f ; g = h`.

Switching modes empties the unknown and fills the two given functions with random ones.
Every function stays editable in every mode: click an element of `A` or `B`, then its
image (an element of `B` sets `f`, of `C` sets `h` or `g`). The page reports unmapped
elements, where the triangle fails to commute, and for the unknown function whether a
solution exists and how many there are. Random functions (any, injective, surjective,
bijective) and a random solution are drawn from a seed kept in the model, so that every
message is one synchronous state change (`Main` mirrors each change into the URL; several
changes in quick succession, as with `Random.generate` commands, would race with the URL
echoes).

Unlike the chapters, the playground always uses the diagrammatic composition order.

-}

import Array exposing (Array)
import Html exposing (Html, button, div, input, p, span, text)
import Html.Attributes exposing (class, classList, disabled, style, title, type_, value)
import Html.Events exposing (onClick, onInput)
import KaTeX
import Math.Crossings as Crossings
import Math.Triangle as Triangle exposing (Kind(..), Partial, Role(..), Triangle)
import Query exposing (Query)
import Random
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import View.ArrowHead as ArrowHead


type Mode
    = Composition
    | Extension
    | Lifting


type SetId
    = SetA
    | SetB
    | SetC


type alias Model =
    { mode : Mode
    , triangle : Triangle
    , selected : Maybe ( SetId, Int )
    , seed : Random.Seed
    , order : Array (List Int) -- per set (A, B, C): the element drawn in each slot
    }


type Msg
    = SetMode Mode
    | SetSize SetId Int
    | ClickElement SetId Int
    | Randomize Role Kind
    | Solve
    | ClearUnknown
    | Untangle
    | ResetOrder


maxSize : Int
maxSize =
    6


init : Model
init =
    let
        f =
            Array.fromList [ Just 0, Just 1, Just 1 ]

        g =
            Array.fromList [ Just 0, Just 1, Just 1 ]
    in
    { mode = Composition
    , triangle = { a = 3, b = 3, c = 2, f = f, g = g, h = Triangle.empty 3 }
    , selected = Nothing
    , seed = Random.initialSeed 0
    , order = identityOrders { a = 3, b = 3, c = 2 }
    }


{-| Start the random choices from a seed (`Main` passes one from its flags).
-}
withSeed : Int -> Model -> Model
withSeed n model =
    { model | seed = Random.initialSeed n }


{-| The function the current mode asks for.
-}
unknown : Mode -> Role
unknown mode =
    case mode of
        Composition ->
            H

        Extension ->
            G

        Lifting ->
            F



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        t =
            model.triangle
    in
    case msg of
        SetMode mode ->
            -- also for the current mode: start it over with new random given functions
            let
                ( given1, given2 ) =
                    givens mode

                cleared =
                    { model | mode = mode, selected = Nothing, triangle = List.foldl Triangle.clear t [ F, G, H ] }
            in
            cleared |> randomize given1 AnyFunction |> randomize given2 AnyFunction

        SetSize set n ->
            let
                resized =
                    resizeTo set (clamp 0 maxSize n) t
            in
            { model | selected = Nothing, triangle = resized, order = fitOrders resized model.order }

        Untangle ->
            { model | order = Crossings.minimize (layout model) model.order }

        ResetOrder ->
            { model | order = identityOrders t }

        ClickElement set i ->
            clickElement set i model

        Randomize role kind ->
            randomize role kind model

        Solve ->
            let
                role =
                    unknown model.mode
            in
            case Triangle.solutionOptions role t of
                Just options ->
                    if Triangle.countSolutions options > 0 then
                        draw role (Triangle.randomSolution options) model

                    else
                        model

                Nothing ->
                    model

        ClearUnknown ->
            { model | selected = Nothing, triangle = Triangle.clear (unknown model.mode) t }


{-| Replace a function by a random one of the given kind, if the set sizes allow one.
-}
randomize : Role -> Kind -> Model -> Model
randomize role kind model =
    let
        ( n, m ) =
            Triangle.sizes role model.triangle
    in
    if Triangle.kindAllowed kind n m then
        draw role (Triangle.randomFunction kind n m) model

    else
        model


{-| Replace a function by one drawn from the generator, advancing the seed.
-}
draw : Role -> Random.Generator Partial -> Model -> Model
draw role generator model =
    let
        ( p, seed ) =
            Random.step generator model.seed
    in
    { model | selected = Nothing, seed = seed, triangle = Triangle.set role p model.triangle }


{-| The two functions the current mode gives.
-}
givens : Mode -> ( Role, Role )
givens mode =
    case mode of
        Composition ->
            ( F, G )

        Extension ->
            ( F, H )

        Lifting ->
            ( G, H )


resizeTo : SetId -> Int -> Triangle -> Triangle
resizeTo set n t =
    let
        sized =
            case set of
                SetA ->
                    { t | a = n }

                SetB ->
                    { t | b = n }

                SetC ->
                    { t | c = n }
    in
    { sized
        | f = Triangle.resize sized.a sized.b t.f
        , g = Triangle.resize sized.b sized.c t.g
        , h = Triangle.resize sized.a sized.c t.h
    }


{-| Select a source element (of `A` or `B`), or set the image of the selected one: an
element of `B` is `f`'s image of an element of `A`, an element of `C` is `h`'s (from `A`)
or `g`'s (from `B`).
-}
clickElement : SetId -> Int -> Model -> Model
clickElement set i model =
    let
        assign role src =
            let
                t =
                    model.triangle
            in
            { model
                | selected = Nothing
                , triangle = Triangle.set role (Array.set src (Just i) (Triangle.get role t)) t
            }
    in
    case ( model.selected, set ) of
        ( Just ( SetA, src ), SetB ) ->
            assign F src

        ( Just ( SetA, src ), SetC ) ->
            assign H src

        ( Just ( SetB, src ), SetC ) ->
            assign G src

        ( _, SetC ) ->
            model

        _ ->
            let
                toggle =
                    if model.selected == Just ( set, i ) then
                        { model | selected = Nothing }

                    else
                        { model | selected = Just ( set, i ) }
            in
            toggle



-- QUERY


modeKey : Mode -> String
modeKey mode =
    case mode of
        Composition ->
            "composition"

        Extension ->
            "extension"

        Lifting ->
            "lifting"


{-| `m` the mode, `a`, `b`, `c` the set sizes, `f`, `g`, `h` the mappings (-1 = unmapped).
-}
toQuery : Model -> List ( String, String )
toQuery model =
    let
        t =
            model.triangle

        mapping p =
            Array.toList p |> List.map (Maybe.withDefault -1)
    in
    [ Query.param "m" (modeKey model.mode)
    , Query.param "a" (String.fromInt t.a)
    , Query.param "b" (String.fromInt t.b)
    , Query.param "c" (String.fromInt t.c)
    , Query.intListParam "f" (mapping t.f)
    , Query.intListParam "g" (mapping t.g)
    , Query.intListParam "h" (mapping t.h)
    ]


fromQuery : Query -> Model -> Model
fromQuery q model =
    let
        t =
            model.triangle

        mode =
            case Query.string "m" q of
                Just "composition" ->
                    Composition

                Just "extension" ->
                    Extension

                Just "lifting" ->
                    Lifting

                _ ->
                    model.mode

        size key default =
            Query.int key q |> Maybe.withDefault default |> clamp 0 maxSize

        ( a, b, c ) =
            ( size "a" t.a, size "b" t.b, size "c" t.c )

        mapping key n m old =
            case Query.intList key q of
                Just xs ->
                    xs
                        |> List.map
                            (\j ->
                                if j >= 0 then
                                    Just j

                                else
                                    Nothing
                            )
                        |> Array.fromList
                        |> Triangle.resize n m

                Nothing ->
                    Triangle.resize n m old
    in
    { mode = mode
    , selected = Nothing
    , seed = model.seed
    , order = fitOrders { a = a, b = b, c = c } model.order
    , triangle =
        { a = a
        , b = b
        , c = c
        , f = mapping "f" a b t.f
        , g = mapping "g" b c t.g
        , h = mapping "h" a c t.h
        }
    }



-- ELEMENT ORDER


setIndex : SetId -> Int
setIndex set =
    case set of
        SetA ->
            0

        SetB ->
            1

        SetC ->
            2


identityOrders : { r | a : Int, b : Int, c : Int } -> Array (List Int)
identityOrders sizes =
    Array.fromList [ List.range 0 (sizes.a - 1), List.range 0 (sizes.b - 1), List.range 0 (sizes.c - 1) ]


{-| Adapt the element orders to new set sizes: removed elements leave their slots, new
ones are appended at the end.
-}
fitOrders : { r | a : Int, b : Int, c : Int } -> Array (List Int) -> Array (List Int)
fitOrders sizes orders =
    let
        fit i n =
            let
                kept =
                    Array.get i orders |> Maybe.withDefault [] |> List.filter (\e -> e < n)
            in
            kept ++ List.filter (\e -> not (List.member e kept)) (List.range 0 (n - 1))
    in
    Array.fromList [ fit 0 sizes.a, fit 1 sizes.b, fit 2 sizes.c ]


{-| The slot element `i` of a set is drawn in.
-}
slotOf : Model -> SetId -> Int -> Int
slotOf model set i =
    Array.get (setIndex set) model.order
        |> Maybe.andThen (\order -> List.indexedMap Tuple.pair order |> List.filter (\( _, e ) -> e == i) |> List.head)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault i


{-| Where element `i` of a set is drawn.
-}
elementPoint : Model -> SetId -> Int -> ( Float, Float )
elementPoint model set i =
    position set (sizeOf set model.triangle) (slotOf model set i)


{-| The picture's slots and arrows, for counting and minimizing crossings.
-}
layout : Model -> Crossings.Layout
layout model =
    let
        t =
            model.triangle

        sets =
            [ SetA, SetB, SetC ]

        slotPosition s slot =
            case List.drop s sets of
                set :: _ ->
                    position set (sizeOf set t) slot

                [] ->
                    ( 0, 0 )

        arrowsOf role =
            let
                ( src, tgt ) =
                    roleSets role
            in
            Array.toIndexedList (Triangle.get role t)
                |> List.filterMap (\( i, mj ) -> Maybe.map (\j -> ( ( setIndex src, i ), ( setIndex tgt, j ) )) mj)
    in
    { position = slotPosition, arrows = List.concatMap arrowsOf [ F, G, H ] }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "ct-page" ]
        [ div [ class "ct-side" ]
            [ goalBar model
            , statusLine model
            , toolbar model
            ]
        , div [ class "ct-main" ] [ stage model ]
        ]


{-| Reordering the elements to untangle the arrows, and the click hint.
-}
toolbar : Model -> Html Msg
toolbar model =
    let
        crossings =
            Crossings.count (layout model) model.order
    in
    div [ class "ct-section" ]
        [ div [ class "controls" ]
            [ button
                [ onClick Untangle
                , disabled (crossings == 0)
                , title "Reorder the elements within each set to reduce the number of crossing arrows"
                ]
                [ text "Minimize crossings" ]
            , button
                [ onClick ResetOrder
                , disabled (model.order == identityOrders model.triangle)
                , title "Draw the elements of every set in the order 1, 2, 3, …"
                ]
                [ text "Reset element order" ]
            , span [ class "muted" ]
                [ text
                    (if crossings == 1 then
                        "1 crossing"

                     else
                        String.fromInt crossings ++ " crossings"
                    )
                ]
            ]
        , p [ class "muted ct-hint" ] [ text (hint model) ]
        ]


roleName : Role -> String
roleName role =
    case role of
        F ->
            "f"

        G ->
            "g"

        H ->
            "h"


roleColor : Role -> String
roleColor role =
    case role of
        F ->
            "#1f77b4"

        G ->
            "#d35400"

        H ->
            "#2e8b57"


setName : SetId -> String
setName set =
    case set of
        SetA ->
            "A"

        SetB ->
            "B"

        SetC ->
            "C"


roleSets : Role -> ( SetId, SetId )
roleSets role =
    case role of
        F ->
            ( SetA, SetB )

        G ->
            ( SetB, SetC )

        H ->
            ( SetA, SetC )


{-| `f`, `g` or `h` in its color, as TeX.
-}
fn : Role -> String
fn role =
    "\\textcolor{" ++ roleColor role ++ "}{" ++ roleName role ++ "}"


{-| An element by its displayed name (indices start at 0, names at 1).
-}
el : Int -> String
el i =
    String.fromInt (i + 1)


signature : Role -> String
signature role =
    let
        ( s, t ) =
            roleSets role
    in
    fn role ++ " : " ++ setName s ++ " \\to " ++ setName t


tex : String -> Html msg
tex =
    KaTeX.inline


goalBar : Model -> Html Msg
goalBar model =
    let
        t =
            model.triangle

        role =
            unknown model.mode

        ( given1, given2 ) =
            givens model.mode

        wanted =
            sought model.mode

        modeButton mode label =
            button [ classList [ ( "active", model.mode == mode ) ], onClick (SetMode mode), title "Start this mode with random given functions" ] [ text label ]

        options =
            Triangle.solutionOptions role t

        count =
            Maybe.map Triangle.countSolutions options |> Maybe.withDefault 0

        status =
            case options of
                Nothing ->
                    p [ class "muted" ]
                        ([ text "Complete "
                         , tex (fn given1)
                         , text " and "
                         , tex (fn given2)
                         , text (" to see whether " ++ wanted.article ++ wanted.one)
                         ]
                            ++ wanted.of_
                            ++ [ text " exists." ]
                        )

                Just opts ->
                    if count == 0 then
                        p [ class "cc-error" ] (noSolution role t)

                    else
                        p []
                            (span [ class "badge ok" ]
                                (text
                                    (if count == 1 then
                                        "exactly 1 " ++ wanted.one

                                     else
                                        String.fromInt count ++ " " ++ wanted.many
                                    )
                                    :: wanted.of_
                                )
                                :: solutionCount role t opts
                                ++ (if Triangle.total (Triangle.get role t) /= Nothing && List.isEmpty (Triangle.commutationFailures t) then
                                        [ span [ class "cc-ok" ] [ text " ✓ Your ", tex (fn role), text " is one of them." ] ]

                                    else
                                        []
                                   )
                            )
    in
    div [ class "ct-goal" ]
        [ div [ class "controls" ]
            [ modeButton Composition "Composition"
            , modeButton Extension "Extension"
            , modeButton Lifting "Lifting"
            ]
        , p []
            ([ text "Given "
             , tex (fn given1)
             , text " and "
             , tex (fn given2)
             , text ", find "
             , tex (fn role)
             , text " with "
             , tex (fn F ++ " ; " ++ fn G ++ " = " ++ fn H)
             , text (", " ++ wanted.article ++ wanted.one)
             ]
                ++ wanted.of_
                ++ [ text "." ]
            )
        , status
        , div [ class "controls" ]
            [ button [ class "primary", onClick Solve, disabled (count == 0) ]
                [ text
                    (if model.mode == Composition then
                        "Fill in the composite"

                     else
                        "Pick a random " ++ wanted.one
                    )
                ]
            , button [ onClick ClearUnknown ] [ tex ("\\text{Clear } " ++ fn role) ]
            ]
        ]


{-| What the mode asks for, e.g. "an", "extension", "extensions" and "of h along f".
-}
sought : Mode -> { article : String, one : String, many : String, of_ : List (Html msg) }
sought mode =
    case mode of
        Composition ->
            { article = "the ", one = "composite", many = "composites", of_ = [ text " of ", tex (fn F), text " and ", tex (fn G) ] }

        Extension ->
            { article = "an ", one = "extension", many = "extensions", of_ = [ text " of ", tex (fn H), text " along ", tex (fn F) ] }

        Lifting ->
            { article = "a ", one = "lifting", many = "liftings", of_ = [ text " of ", tex (fn H), text " along ", tex (fn G) ] }


{-| Why the unknown function cannot exist.
-}
noSolution : Role -> Triangle -> List (Html msg)
noSolution role t =
    let
        at p i =
            Array.get i p |> Maybe.andThen identity |> Maybe.withDefault 0
    in
    case ( role, Triangle.conflict role t ) of
        ( G, Just ( i, k ) ) ->
            [ text "No extension of "
            , tex (fn H)
            , text " along "
            , tex (fn F)
            , text " exists: "
            , tex (fn F ++ "(" ++ el i ++ ") = " ++ fn F ++ "(" ++ el k ++ ") = " ++ el (at t.f i))
            , text " but "
            , tex (fn H ++ "(" ++ el i ++ ") = " ++ el (at t.h i) ++ " \\neq " ++ el (at t.h k) ++ " = " ++ fn H ++ "(" ++ el k ++ ")")
            , text ". Whatever g does, "
            , tex (fn G ++ "(" ++ fn F ++ "(" ++ el i ++ ")) = " ++ fn G ++ "(" ++ fn F ++ "(" ++ el k ++ "))")
            , text "."
            ]

        ( F, Just ( i, _ ) ) ->
            [ text "No lifting of "
            , tex (fn H)
            , text " along "
            , tex (fn G)
            , text " exists: "
            , tex (fn H ++ "(" ++ el i ++ ") = " ++ el (at t.h i))
            , text " is not in the image of "
            , tex (fn G)
            , text ", so no choice of "
            , tex (fn F ++ "(" ++ el i ++ ")")
            , text " gives "
            , tex (fn G ++ "(" ++ fn F ++ "(" ++ el i ++ ")) = " ++ el (at t.h i))
            , text "."
            ]

        _ ->
            [ text "No solution exists." ]


{-| How the number of solutions comes about.
-}
solutionCount : Role -> Triangle -> List (List Int) -> List (Html msg)
solutionCount role t options =
    case role of
        G ->
            let
                free =
                    List.range 0 (t.b - 1)
                        |> List.filter (\j -> not (List.member (Just j) (Array.toList t.f)))
                        |> List.length

                forced =
                    t.b - free
            in
            [ tex (fn G)
            , text (" is forced on the image of f (" ++ String.fromInt forced ++ " of " ++ String.fromInt t.b ++ " elements of B, ")
            , tex (fn G ++ "(" ++ fn F ++ "(a)) = " ++ fn H ++ "(a)")
            , text ")"
            , if free == 0 then
                text "."

              else
                span []
                    [ text "; the other "
                    , text (String.fromInt free)
                    , text " can go anywhere in C: "
                    , tex ("|C|^{" ++ String.fromInt free ++ "} = " ++ String.fromInt t.c ++ "^{" ++ String.fromInt free ++ "} = " ++ String.fromInt (Triangle.countSolutions options))
                    , text "."
                    ]
            ]

        H ->
            [ tex (fn H)
            , text " is determined by "
            , tex (fn F)
            , text " and "
            , tex (fn G)
            , text ": "
            , tex (fn H ++ "(a) = " ++ fn G ++ "(" ++ fn F ++ "(a))")
            , text "."
            ]

        F ->
            [ text "Each "
            , tex (fn F ++ "(a)")
            , text " can be any element of the fiber "
            , tex (fn G ++ "^{-1}(" ++ fn H ++ "(a))")
            , text ": "
            , tex
                (String.join " \\cdot " (List.map (List.length >> String.fromInt) options)
                    ++ " = "
                    ++ String.fromInt (Triangle.countSolutions options)
                )
            , text "."
            ]


{-| Whether the triangle commutes: one line when it does, the failing elements when not.
Unmapped elements are reported next to their function in the picture.
-}
statusLine : Model -> Html Msg
statusLine model =
    let
        t =
            model.triangle

        failure ( i, x, y ) =
            tex (fn G ++ "(" ++ fn F ++ "(" ++ el i ++ ")) = " ++ el x ++ " \\neq " ++ el y ++ " = " ++ fn H ++ "(" ++ el i ++ ")")
    in
    case Triangle.commutationFailures t of
        [] ->
            if List.all (\role -> List.isEmpty (Triangle.unmapped (Triangle.get role t))) [ F, G, H ] then
                p [ class "ct-status ok" ] [ text "✓ The triangle commutes: ", tex (fn F ++ " ; " ++ fn G ++ " = " ++ fn H) ]

            else
                p [ class "ct-status muted" ] [ text "No disagreement so far; whether the triangle commutes is decided once all three are functions." ]

        failures ->
            p [ class "ct-status bad" ]
                (text "✗ The triangle does not commute: "
                    :: List.intersperse (text ", ") (List.map failure failures)
                )


{-| Overlay attributes placing a control centered at a point of the picture.
-}
placeAt : ( Float, Float ) -> List (Html.Attribute msg)
placeAt ( x, y ) =
    [ style "left" (String.fromFloat (100 * x / pictureWidth) ++ "%")
    , style "top" (String.fromFloat (100 * y / pictureHeight) ++ "%")
    ]


setAnchor : SetId -> ( Float, Float )
setAnchor set =
    case set of
        SetA ->
            ( 100, 30 )

        SetB ->
            ( 680, 90 )

        SetC ->
            ( 480, 600 )


functionAnchor : Role -> ( Float, Float )
functionAnchor role =
    case role of
        F ->
            ( 290, 52 )

        G ->
            ( 742, 410 )

        H ->
            ( 165, 545 )


{-| The set's name with − / size / + next to it.
-}
setControl : Triangle -> SetId -> Html Msg
setControl t set =
    let
        n =
            sizeOf set t
    in
    div (class "ct-ctl" :: placeAt (setAnchor set))
        [ div [ class "ct-row" ]
            [ span [ class "ct-set-name" ] [ text (setName set) ]
            , button [ class "small", onClick (SetSize set (n - 1)), disabled (n <= 0), title "Remove the last element" ] [ text "−" ]
            , input
                [ type_ "number"
                , Html.Attributes.min "0"
                , Html.Attributes.max (String.fromInt maxSize)
                , value (String.fromInt n)
                , title ("Number of elements of " ++ setName set)
                , onInput (\str -> SetSize set (String.toInt str |> Maybe.withDefault n))
                ]
                []
            , button [ class "small", onClick (SetSize set (n + 1)), disabled (n >= maxSize), title "Add an element" ] [ text "+" ]
            ]
        ]


{-| The function's signature, whether it is the one to find, its unmapped elements, and
buttons replacing it by a random function of some kind.
-}
functionControl : Model -> Role -> Html Msg
functionControl model role =
    let
        t =
            model.triangle

        ( n, m ) =
            Triangle.sizes role t

        kindButton ( kind, label, long ) =
            let
                allowed =
                    Triangle.kindAllowed kind n m
            in
            button
                [ onClick (Randomize role kind)
                , disabled (not allowed)
                , title
                    (if allowed then
                        "Replace " ++ roleName role ++ " by a random " ++ long ++ " function"

                     else
                        "There is no " ++ long ++ " function from a set of size " ++ String.fromInt n ++ " to one of size " ++ String.fromInt m
                    )
                ]
                [ text label ]
    in
    div (class "ct-ctl" :: placeAt (functionAnchor role))
        [ div [ class "ct-row" ]
            [ tex (signature role)
            , if role == unknown model.mode then
                span [ class "ct-tag" ] [ text "to find" ]

              else
                text ""
            ]
        , case Triangle.unmapped (Triangle.get role t) of
            [] ->
                text ""

            missing ->
                let
                    ( src, _ ) =
                        roleSets role
                in
                span [ class "ct-warn", title (roleName role ++ " is not a function until every element of " ++ setName src ++ " is mapped") ]
                    [ text ("⚠ " ++ String.join ", " (List.map el missing) ++ " ∈ " ++ setName src ++ " not mapped") ]
        , div [ class "ct-row ct-dice" ]
            (span [ class "muted", title "Random function" ] [ text "random:" ]
                :: List.map kindButton
                    [ ( AnyFunction, "any", "arbitrary" )
                    , ( Injective, "inj", "injective" )
                    , ( Surjective, "surj", "surjective" )
                    , ( Bijective, "bij", "bijective" )
                    ]
            )
        ]



-- PICTURE


radius : Float
radius =
    14


spacing : Float
spacing =
    46


{-| Position of element `i` of a set of size `n`: `A` a column, `B` a diagonal, `C` a row,
so that every function's arrows leave and enter their sets from one side.
-}
position : SetId -> Int -> Int -> ( Float, Float )
position set n i =
    let
        k =
            toFloat i - toFloat (n - 1) / 2
    in
    case set of
        SetA ->
            ( 100, 200 + k * spacing )

        SetB ->
            ( 500 + k * 50 / sqrt 2, 200 + k * 50 / sqrt 2 )

        SetC ->
            ( 480 + k * spacing, 540 )


center : SetId -> ( Float, Float )
center set =
    position set 1 0


sizeOf : SetId -> Triangle -> Int
sizeOf set t =
    case set of
        SetA ->
            t.a

        SetB ->
            t.b

        SetC ->
            t.c


pictureWidth : Float
pictureWidth =
    860


pictureHeight : Float
pictureHeight =
    630


{-| The picture with the set and function controls laid over it next to their names.
-}
stage : Model -> Html Msg
stage model =
    div [ class "ct-stage" ]
        (Svg.svg [ SA.viewBox ("0 0 " ++ String.fromFloat pictureWidth ++ " " ++ String.fromFloat pictureHeight) ]
            (List.map (setShape model.triangle) [ SetA, SetB, SetC ]
                ++ List.concatMap (arrows model) [ H, F, G ]
                ++ List.concatMap (elements model) [ SetA, SetB, SetC ]
            )
            :: List.map (setControl model.triangle) [ SetA, SetB, SetC ]
            ++ List.map (functionControl model) [ F, G, H ]
        )


hint : Model -> String
hint model =
    case model.selected of
        Just ( SetA, i ) ->
            "Now click the image of " ++ el i ++ " ∈ A: an element of B sets f(" ++ el i ++ "), an element of C sets h(" ++ el i ++ ")."

        Just ( SetB, i ) ->
            "Now click the image of " ++ el i ++ " ∈ B in C to set g(" ++ el i ++ ")."

        _ ->
            "Click an element of A or B, then its image in B or C. Dashed arrows belong to the function to find."


setShape : Triangle -> SetId -> Svg msg
setShape t set =
    let
        n =
            sizeOf set t

        ( x1, y1 ) =
            position set n 0

        ( x2, y2 ) =
            position set n (n - 1)

        capsule width color =
            Svg.line
                [ SA.x1 (String.fromFloat x1)
                , SA.y1 (String.fromFloat y1)
                , SA.x2 (String.fromFloat x2)
                , SA.y2 (String.fromFloat y2)
                , SA.stroke color
                , SA.strokeWidth (String.fromFloat width)
                , SA.strokeLinecap "round"
                ]
                []
    in
    if n == 0 then
        let
            ( cx, cy ) =
                center set
        in
        Svg.g []
            [ Svg.circle [ SA.cx (String.fromFloat cx), SA.cy (String.fromFloat cy), SA.r "22", SA.fill "#f3f7fa", SA.stroke "#c5d0d8" ] []
            , Svg.text_ [ SA.x (String.fromFloat cx), SA.y (String.fromFloat (cy + 6)), SA.textAnchor "middle", SA.fill "#999", SA.fontSize "18" ] [ Svg.text "∅" ]
            ]

    else
        Svg.g [] [ capsule 46 "#c5d0d8", capsule 44 "#f3f7fa" ]


arrows : Model -> Role -> List (Svg msg)
arrows model role =
    let
        t =
            model.triangle

        ( s, d ) =
            roleSets role

        dashed =
            role == unknown model.mode

        arrow ( i, mj ) =
            case mj of
                Just j ->
                    let
                        ( x1, y1 ) =
                            elementPoint model s i

                        ( x2, y2 ) =
                            elementPoint model d j

                        len =
                            max 0.001 (sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))

                        ( ux, uy ) =
                            ( (x2 - x1) / len, (y2 - y1) / len )

                        start =
                            ( x1 + ux * (radius + 2), y1 + uy * (radius + 2) )

                        tip =
                            ( x2 - ux * (radius + 2), y2 - uy * (radius + 2) )

                        emphasized =
                            model.selected == Just ( s, i )
                    in
                    [ Svg.line
                        ([ SA.x1 (String.fromFloat (Tuple.first start))
                         , SA.y1 (String.fromFloat (Tuple.second start))
                         , SA.x2 (String.fromFloat (Tuple.first tip - ux * 8))
                         , SA.y2 (String.fromFloat (Tuple.second tip - uy * 8))
                         , SA.stroke (roleColor role)
                         , SA.strokeWidth
                            (if emphasized then
                                "3.5"

                             else
                                "2"
                            )
                         , style "pointer-events" "none"
                         ]
                            ++ (if dashed then
                                    [ SA.strokeDasharray "6 4" ]

                                else
                                    []
                               )
                        )
                        []
                    , ArrowHead.view { tip = tip, from = start, size = 10, color = roleColor role }
                    ]

                Nothing ->
                    []
    in
    Array.toIndexedList (Triangle.get role t) |> List.concatMap arrow


elements : Model -> SetId -> List (Svg Msg)
elements model set =
    let
        t =
            model.triangle

        n =
            sizeOf set t

        failing =
            Triangle.commutationFailures t |> List.map (\( i, _, _ ) -> i)

        -- the function an element of this set would be the image under, given the selection
        targetRole =
            case ( model.selected, set ) of
                ( Just ( SetA, _ ), SetB ) ->
                    Just F

                ( Just ( SetA, _ ), SetC ) ->
                    Just H

                ( Just ( SetB, _ ), SetC ) ->
                    Just G

                _ ->
                    Nothing

        clickable =
            set /= SetC || targetRole /= Nothing

        element i =
            let
                ( x, y ) =
                    elementPoint model set i

                selected =
                    model.selected == Just ( set, i )

                fails =
                    set == SetA && List.member i failing

                stroke =
                    if selected then
                        "#222"

                    else
                        case targetRole of
                            Just role ->
                                roleColor role

                            Nothing ->
                                if fails then
                                    "#c0392b"

                                else
                                    "#555"
            in
            Svg.g
                (style "cursor"
                    (if clickable then
                        "pointer"

                     else
                        "default"
                    )
                    :: (if clickable then
                            [ Svg.Events.onClick (ClickElement set i) ]

                        else
                            []
                       )
                )
                [ Svg.title [] [ Svg.text (el i ++ " ∈ " ++ setName set) ]
                , if fails then
                    Svg.circle [ SA.cx (String.fromFloat x), SA.cy (String.fromFloat y), SA.r (String.fromFloat (radius + 5)), SA.fill "none", SA.stroke "#c0392b", SA.strokeWidth "2", SA.strokeDasharray "3 2" ] []

                  else
                    Svg.text ""
                , Svg.circle
                    [ SA.cx (String.fromFloat x)
                    , SA.cy (String.fromFloat y)
                    , SA.r (String.fromFloat radius)
                    , SA.fill
                        (if selected then
                            "#ffd54f"

                         else
                            "#fff"
                        )
                    , SA.stroke stroke
                    , SA.strokeWidth
                        (if selected || targetRole /= Nothing then
                            "3"

                         else
                            "1.5"
                        )
                    ]
                    []
                , Svg.text_
                    [ SA.x (String.fromFloat x)
                    , SA.y (String.fromFloat (y + 5))
                    , SA.textAnchor "middle"
                    , SA.fontSize "14"
                    , SA.fill "#222"
                    , style "pointer-events" "none"
                    ]
                    [ Svg.text (el i) ]
                ]
    in
    List.map element (List.range 0 (n - 1))
