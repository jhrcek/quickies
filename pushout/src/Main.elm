module Main exposing (..)

import Array
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Force
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Random
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- CONSTANTS


canvasW : Float
canvasW =
    800


canvasH : Float
canvasH =
    600


quadA : { x : Float, y : Float }
quadA =
    { x = 200, y = 150 }


quadB1 : { x : Float, y : Float }
quadB1 =
    { x = 600, y = 150 }


quadB2 : { x : Float, y : Float }
quadB2 =
    { x = 200, y = 450 }


quadP : { x : Float, y : Float }
quadP =
    { x = 600, y = 450 }


canvasCenter : Point
canvasCenter =
    { x = canvasW / 2, y = canvasH / 2 }


type alias Point =
    { x : Float, y : Float }


{-| A line segment along which the four-quadrant view lays out nodes,
running from the outer corner of a quadrant towards the canvas center.
-}
type alias Diagonal =
    { from : Point, to : Point }


diagonalA : Diagonal
diagonalA =
    { from = { x = 50, y = 40 }, to = { x = 350, y = 260 } }


diagonalB1 : Diagonal
diagonalB1 =
    { from = { x = 770, y = 40 }, to = { x = 430, y = 260 } }


diagonalB2 : Diagonal
diagonalB2 =
    { from = { x = 50, y = 570 }, to = { x = 350, y = 350 } }


diagonalP : Diagonal
diagonalP =
    { from = { x = 770, y = 560 }, to = { x = 430, y = 340 } }


pointAlong : Diagonal -> Float -> Point
pointAlong { from, to } t =
    { x = from.x + t * (to.x - from.x), y = from.y + t * (to.y - from.y) }


{-| Parameter in (0, 1) for the item at `rank` when spreading `size` items evenly.
-}
spreadT : Int -> Int -> Float
spreadT size rank =
    if size <= 1 then
        0.5

    else
        (toFloat rank + 0.5) / toFloat size


goldenAngle : Float
goldenAngle =
    pi * (3 - sqrt 5)


colorA : String
colorA =
    "#44aa44"


colorB1 : String
colorB1 =
    "#4488cc"


colorB2 : String
colorB2 =
    "#cc4444"


colorPB1 : String
colorPB1 =
    "#7ab8f0"


colorPB2 : String
colorPB2 =
    "#e88888"


{-| Convert client (mouse) coordinates to SVG viewBox coordinates,
accounting for the SVG element's position, uniform scaling, and
letterboxing from preserveAspectRatio (default xMidYMid meet).
-}
clientToSvg : { a | svgOffset : { x : Float, y : Float, width : Float, height : Float } } -> ( Float, Float ) -> ( Float, Float )
clientToSvg model ( cx, cy ) =
    let
        -- The SVG uses uniform scaling (preserveAspectRatio meet),
        -- so the scale factor is determined by the tighter dimension.
        scaleX =
            model.svgOffset.width / canvasW

        scaleY =
            model.svgOffset.height / canvasH

        scale =
            min scaleX scaleY

        -- The rendered content size in client pixels
        renderedW =
            canvasW * scale

        renderedH =
            canvasH * scale

        -- Letterboxing offsets (content is centered within the SVG element)
        padX =
            (model.svgOffset.width - renderedW) / 2

        padY =
            (model.svgOffset.height - renderedH) / 2
    in
    ( (cx - model.svgOffset.x - padX) / scale
    , (cy - model.svgOffset.y - padY) / scale
    )



-- MODEL


{-| Maps node IDs to their SVG coordinates.
-}
type alias NodePositions =
    Dict String { x : Float, y : Float }


type alias Model =
    { sizeA : Int
    , sizeB1 : Int
    , sizeB2 : Int
    , funcF : List Int
    , funcG : List Int
    , equivClasses : List (List String)
    , entities : List ForceEntity
    , forceState : Force.State String
    , currentView : ViewMode
    , drag : Maybe DragState
    , svgOffset : { x : Float, y : Float, width : Float, height : Float }
    , transition : Maybe Transition
    , simulationPaused : Bool
    , colorMode : ColorMode
    }


type alias Transition =
    { startPositions : NodePositions
    , targetPositions : NodePositions
    , progress : Float -- 0.0 to 1.0
    , phase : TransitionPhase
    }


{-| During a merge phase, B1/B2 are visible and FQ-style arrows are shown.
During a rearrange phase, B1/B2 are hidden and ZP-style arrows are shown.
-}
type TransitionPhase
    = MergePhase { nextTargets : NodePositions }
    | RearrangePhase { nextTargets : NodePositions }
    | FinalPhase


phaseNextTargets : TransitionPhase -> Maybe NodePositions
phaseNextTargets phase =
    case phase of
        MergePhase { nextTargets } ->
            Just nextTargets

        RearrangePhase { nextTargets } ->
            Just nextTargets

        FinalPhase ->
            Nothing


type alias ForceEntity =
    Force.Entity String { value : EntityData }


type alias EntityData =
    { group : EntityGroup
    , index : Int
    , label : String
    }


type EntityGroup
    = GroupA
    | GroupB1
    | GroupB2
    | GroupPB1
    | GroupPB2


{-| Force.Entity requires a comparable id, so each node is identified by a string
built from its group and index. Build ids only through this function.
-}
nodeId : EntityGroup -> Int -> String
nodeId group i =
    let
        prefix =
            case group of
                GroupA ->
                    "a_"

                GroupB1 ->
                    "b1_"

                GroupB2 ->
                    "b2_"

                GroupPB1 ->
                    "pb1_"

                GroupPB2 ->
                    "pb2_"
    in
    prefix ++ String.fromInt i


{-| The pushout group that the canonical injection maps a B group into.
-}
injectionTarget : EntityGroup -> Maybe EntityGroup
injectionTarget group =
    case group of
        GroupB1 ->
            Just GroupPB1

        GroupB2 ->
            Just GroupPB2

        _ ->
            Nothing


groupLabel : EntityGroup -> Int -> String
groupLabel group i =
    case group of
        GroupA ->
            String.fromInt (i + 1)

        GroupB1 ->
            lowercaseLetter i

        GroupPB1 ->
            lowercaseLetter i

        GroupB2 ->
            uppercaseLetter i

        GroupPB2 ->
            uppercaseLetter i


type ViewMode
    = FourQuadrant
    | ZoomedPushout


type ColorMode
    = ColorBySet
    | ColorByEquivClass


type DragState
    = DraggingArrow { func : WhichFunc, aIndex : Int, mousePos : ( Float, Float ) }
    | DraggingNode { entityId : String, offset : ( Float, Float ), warmState : Force.State String }


type WhichFunc
    = FuncF
    | FuncG


type WhichSet
    = SetA
    | SetB1
    | SetB2


type GenKind
    = GenAny
    | GenInjective
    | GenSurjective



-- MSG


type Msg
    = Tick Float
    | SetSize WhichSet String
    | GotRandomFunctions ( List Int, List Int )
    | SwitchView ViewMode
    | ArrowDragStart WhichFunc Int ( Float, Float )
    | NodeDragStart String ( Float, Float )
    | MouseMove ( Float, Float )
    | MouseUp ( Float, Float )
    | RandomizeFunctions
    | RandomizeSingle WhichFunc GenKind
    | GotRandomSingle WhichFunc (List Int)
    | GotSvgElement (Result Browser.Dom.Error Browser.Dom.Element)
    | ToggleSimulationPaused Bool
    | SetColorMode ColorMode
    | WindowResized



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    let
        sizeA =
            3

        sizeB1 =
            4

        sizeB2 =
            4
    in
    ( { sizeA = sizeA
      , sizeB1 = sizeB1
      , sizeB2 = sizeB2
      , funcF = []
      , funcG = []
      , equivClasses = []
      , entities = []
      , forceState = Force.simulation []
      , currentView = FourQuadrant
      , drag = Nothing
      , svgOffset = { x = 0, y = 0, width = canvasW, height = canvasH }
      , transition = Nothing
      , simulationPaused = False
      , colorMode = ColorBySet
      }
    , Cmd.batch
        [ Random.generate GotRandomFunctions (randomFunctionPair sizeA sizeB1 sizeB2)
        , Browser.Dom.getElement svgId |> Task.attempt GotSvgElement
        ]
    )



-- RANDOM


randomFunctionPair : Int -> Int -> Int -> Random.Generator ( List Int, List Int )
randomFunctionPair sizeA sizeB1 sizeB2 =
    Random.map2 Tuple.pair
        (randomAnyFunction sizeA sizeB1)
        (randomAnyFunction sizeA sizeB2)


randomAnyFunction : Int -> Int -> Random.Generator (List Int)
randomAnyFunction sizeDom sizeCod =
    if sizeDom > 0 && sizeCod > 0 then
        Random.list sizeDom (Random.int 0 (sizeCod - 1))

    else
        Random.constant []


shuffle : List a -> Random.Generator (List a)
shuffle xs =
    Random.list (List.length xs) (Random.float 0 1)
        |> Random.map
            (\ks ->
                List.map2 Tuple.pair ks xs
                    |> List.sortBy Tuple.first
                    |> List.map Tuple.second
            )


randomInjectiveFunction : Int -> Int -> Random.Generator (List Int)
randomInjectiveFunction sizeDom sizeCod =
    shuffle (List.range 0 (sizeCod - 1))
        |> Random.map (List.take sizeDom)


randomSurjectiveFunction : Int -> Int -> Random.Generator (List Int)
randomSurjectiveFunction sizeDom sizeCod =
    if sizeDom == 0 then
        Random.constant []

    else
        Random.list (sizeDom - sizeCod) (Random.int 0 (sizeCod - 1))
            |> Random.andThen (\extras -> shuffle (List.range 0 (sizeCod - 1) ++ extras))


randomFunction : GenKind -> Int -> Int -> Random.Generator (List Int)
randomFunction kind sizeDom sizeCod =
    case kind of
        GenAny ->
            randomAnyFunction sizeDom sizeCod

        GenInjective ->
            randomInjectiveFunction sizeDom sizeCod

        GenSurjective ->
            randomSurjectiveFunction sizeDom sizeCod


canGenerate : GenKind -> Int -> Int -> Bool
canGenerate kind sizeDom sizeCod =
    case kind of
        GenAny ->
            sizeDom == 0 || sizeCod > 0

        GenInjective ->
            sizeDom <= sizeCod

        GenSurjective ->
            if sizeCod == 0 then
                sizeDom == 0

            else
                sizeDom >= sizeCod



-- PUSHOUT


computeEquivClasses : Int -> Int -> List Int -> List Int -> List (List String)
computeEquivClasses sizeB1 sizeB2 funcF funcG =
    let
        -- Start with singleton classes for each pb1_j and pb2_k
        initial =
            List.map (\j -> [ nodeId GroupPB1 j ]) (List.range 0 (sizeB1 - 1))
                ++ List.map (\k -> [ nodeId GroupPB2 k ]) (List.range 0 (sizeB2 - 1))

        -- For each a_i, merge class of pb1_{f(i)} with class of pb2_{g(i)}
        identifications =
            List.map2
                (\fi gi ->
                    ( nodeId GroupPB1 fi
                    , nodeId GroupPB2 gi
                    )
                )
                funcF
                funcG
    in
    List.foldl mergeClasses initial identifications


mergeClasses : ( String, String ) -> List (List String) -> List (List String)
mergeClasses ( a, b ) classes =
    let
        classOfA =
            List.filter (List.member a) classes |> List.head

        classOfB =
            List.filter (List.member b) classes |> List.head
    in
    case ( classOfA, classOfB ) of
        ( Just cA, Just cB ) ->
            if cA == cB then
                classes

            else
                let
                    merged =
                        cA ++ cB

                    remaining =
                        List.filter (\c -> c /= cA && c /= cB) classes
                in
                merged :: remaining

        _ ->
            classes



-- ENTITY + FORCE


{-| Compute orderings of A, B1 and B2 along their diagonals (each from outer corner → center).

Equivalence classes are laid out one after another, larger ones near the outer corners
(the same order as the blobs along P's diagonal). Within a class, the A → B1 and A → B2
arrows can be drawn without crossings whenever that is possible at all (see
caterpillarOrder); otherwise a barycenter heuristic keeps crossings low.

-}
computeEquivClassOrderings : Int -> Int -> Int -> List Int -> List Int -> List (List String) -> { aOrder : List Int, b1Order : List Int, b2Order : List Int }
computeEquivClassOrderings sizeA sizeB1 sizeB2 funcF funcG equivClasses =
    let
        heuristic =
            barycenterOrderings sizeA sizeB1 sizeB2 funcF funcG equivClasses

        classRank =
            classRanks equivClasses

        rankOf group i =
            Dict.get (nodeId group i) classRank |> Maybe.withDefault -1

        fArr =
            Array.fromList funcF

        gArr =
            Array.fromList funcG

        lookup arr i =
            Array.get i arr |> Maybe.withDefault 0

        -- The heuristic orderings are grouped by class rank, so each class is a contiguous slice
        orderClass rank =
            let
                inClass group indexOf =
                    List.filter (\i -> rankOf group (indexOf i) == rank)

                aOrder =
                    inClass GroupPB1 (lookup fArr) heuristic.aOrder

                b1Order =
                    inClass GroupPB1 identity heuristic.b1Order

                b2Order =
                    inClass GroupPB2 identity heuristic.b2Order
            in
            caterpillarOrder b1Order b2Order (List.map (\i -> ( i, ( lookup fArr i, lookup gArr i ) )) aOrder)
                |> Maybe.withDefault { aOrder = aOrder, b1Order = b1Order, b2Order = b2Order }

        perClass =
            List.map orderClass (List.range 0 (List.length equivClasses - 1))
    in
    { aOrder = List.concatMap .aOrder perClass
    , b1Order = List.concatMap .b1Order perClass
    , b2Order = List.concatMap .b2Order perClass
    }


{-| Rank of each pushout node's class in sortClassesBySize order.
-}
classRanks : List (List String) -> Dict String Int
classRanks equivClasses =
    sortClassesBySize equivClasses
        |> List.indexedMap (\ci cls -> List.map (\memberId -> ( memberId, ci )) cls)
        |> List.concat
        |> Dict.fromList


{-| Crossing-free ordering of one equivalence class, if one exists.

View the class as a bipartite graph: vertices are its B1 and B2 elements, and each
a ∈ A is an edge between f(a) and g(a). Arrows A → B1 don't cross iff f is monotone,
i.e. every f-fiber is a contiguous run in A's order; likewise for g. So we need a
linear order of the edges in which each vertex's edges are consecutive. Merging
parallel edges (elements with the same images), such an order exists iff the graph
is a caterpillar: a tree whose non-leaf vertices form a path (the spine). Walking
the spine, emitting each spine vertex's leaf edges and then the edge to the next
spine vertex, gives the order. B1 and B2 are then ordered by their first edge.

Takes the class's B1 elements, B2 elements and A elements with their (f, g) images.

-}
caterpillarOrder : List Int -> List Int -> List ( Int, ( Int, Int ) ) -> Maybe { aOrder : List Int, b1Order : List Int, b2Order : List Int }
caterpillarOrder b1s b2s edges =
    let
        -- A elements bundled by their (f, g) images: the merged parallel edges
        bundles : Dict ( Int, Int ) (List Int)
        bundles =
            List.foldr
                (\( a, link ) -> Dict.update link (\as_ -> Just (a :: Maybe.withDefault [] as_)))
                Dict.empty
                edges

        links =
            Dict.keys bundles

        -- Vertices are tagged by side: ( 1, j ) is B1's j, ( 2, k ) is B2's k
        neighbours : Dict ( Int, Int ) (List ( Int, Int ))
        neighbours =
            List.foldl
                (\( j, k ) ->
                    Dict.update ( 1, j ) (\ns -> Just (( 2, k ) :: Maybe.withDefault [] ns))
                        >> Dict.update ( 2, k ) (\ns -> Just (( 1, j ) :: Maybe.withDefault [] ns))
                )
                Dict.empty
                links

        neighboursOf v =
            Dict.get v neighbours |> Maybe.withDefault [] |> List.sort

        isSpine v =
            List.length (neighboursOf v) >= 2

        spineNeighbours v =
            List.filter isSpine (neighboursOf v)

        linkBetween ( side, i ) ( _, other ) =
            if side == 1 then
                ( i, other )

            else
                ( other, i )

        spineVertices =
            List.filter isSpine (Dict.keys neighbours)

        -- Follow the spine from one of its ends
        walk prev v acc =
            case List.filter (\n -> Just n /= prev) (spineNeighbours v) of
                [] ->
                    List.reverse (v :: acc)

                next :: _ ->
                    walk (Just v) next (v :: acc)

        leafLinks v =
            List.filter (not << isSpine) (neighboursOf v) |> List.map (linkBetween v)
    in
    if List.isEmpty edges then
        -- An unmapped B element in a class of its own
        Just { aOrder = [], b1Order = b1s, b2Order = b2s }

    else if List.length links /= List.length b1s + List.length b2s - 1 then
        -- The class is connected, so this means it has a cycle
        Nothing

    else if List.any (\v -> List.length (spineNeighbours v) > 2) spineVertices then
        -- A tree, but its non-leaf vertices branch instead of forming a path
        Nothing

    else
        let
            orderedLinks =
                case List.filter (\v -> List.length (spineNeighbours v) <= 1) spineVertices of
                    [] ->
                        -- No spine: a single (bundled) edge
                        links

                    start :: _ ->
                        let
                            spine =
                                walk Nothing start []
                        in
                        List.map2
                            (\v next -> leafLinks v ++ [ linkBetween v next ])
                            spine
                            (List.drop 1 spine)
                            ++ List.map leafLinks (List.drop (List.length spine - 1) spine)
                            |> List.concat
        in
        Just
            { aOrder = List.concatMap (\link -> Dict.get link bundles |> Maybe.withDefault []) orderedLinks
            , b1Order = dedupeConsecutive (List.map Tuple.first orderedLinks)
            , b2Order = dedupeConsecutive (List.map Tuple.second orderedLinks)
            }


dedupeConsecutive : List a -> List a
dedupeConsecutive xs =
    List.foldr
        (\x acc ->
            case acc of
                y :: _ ->
                    if x == y then
                        acc

                    else
                        x :: acc

                [] ->
                    [ x ]
        )
        []
        xs


{-| Heuristic orderings: grouped by equivalence class, then within each class
B elements are placed at the mean rank of their preimages and vice versa.
-}
barycenterOrderings : Int -> Int -> Int -> List Int -> List Int -> List (List String) -> { aOrder : List Int, b1Order : List Int, b2Order : List Int }
barycenterOrderings sizeA sizeB1 sizeB2 funcF funcG equivClasses =
    let
        classRank : Dict String Int
        classRank =
            classRanks equivClasses

        b1ClassRank : Int -> Int
        b1ClassRank j =
            Dict.get (nodeId GroupPB1 j) classRank |> Maybe.withDefault 9999

        b2ClassRank : Int -> Int
        b2ClassRank k =
            Dict.get (nodeId GroupPB2 k) classRank |> Maybe.withDefault 9999

        lookupF i =
            List.head (List.drop i funcF) |> Maybe.withDefault 0

        lookupG i =
            List.head (List.drop i funcG) |> Maybe.withDefault 0

        -- Helper: build rank map from an ordering list
        buildRankMap : List Int -> Dict Int Int
        buildRankMap order =
            order
                |> List.indexedMap (\rank origIdx -> ( origIdx, rank ))
                |> Dict.fromList

        -- Helper: compute barycenter (mean A-rank) for a B node's preimage
        barycenter : List Int -> Dict Int Int -> Int -> Float
        barycenter func aRanks targetIdx =
            let
                ranks =
                    func
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap
                            (\( ai, fi ) ->
                                if fi == targetIdx then
                                    Dict.get ai aRanks

                                else
                                    Nothing
                            )
            in
            case ranks of
                [] ->
                    9999

                _ ->
                    toFloat (List.sum ranks) / toFloat (List.length ranks)

        -- Phase 2: Initial A ordering (same primary sort as before)
        aOrder0 =
            List.range 0 (sizeA - 1)
                |> List.sortBy
                    (\i -> ( b1ClassRank (lookupF i), b2ClassRank (lookupG i), i ))

        aRanks0 =
            buildRankMap aOrder0

        -- Phase 3 & 4: B1 and B2 ordering via barycenter
        b1Order0 =
            List.range 0 (sizeB1 - 1)
                |> List.sortBy (\j -> ( b1ClassRank j, barycenter funcF aRanks0 j ))

        b2Order0 =
            List.range 0 (sizeB2 - 1)
                |> List.sortBy (\k -> ( b2ClassRank k, barycenter funcG aRanks0 k ))

        -- Phase 5: Refine A ordering using B1+B2 target ranks
        b1Ranks0 =
            buildRankMap b1Order0

        b2Ranks0 =
            buildRankMap b2Order0

        aOrder1 =
            List.range 0 (sizeA - 1)
                |> List.sortBy
                    (\i ->
                        let
                            fi =
                                lookupF i

                            gi =
                                lookupG i

                            combinedScore =
                                toFloat (Maybe.withDefault 9999 (Dict.get fi b1Ranks0))
                                    + toFloat (Maybe.withDefault 9999 (Dict.get gi b2Ranks0))
                        in
                        ( b1ClassRank fi, b2ClassRank gi, combinedScore )
                    )

        -- Phase 6: Second iteration of B1 and B2
        aRanks1 =
            buildRankMap aOrder1

        b1Order1 =
            List.range 0 (sizeB1 - 1)
                |> List.sortBy (\j -> ( b1ClassRank j, barycenter funcF aRanks1 j ))

        b2Order1 =
            List.range 0 (sizeB2 - 1)
                |> List.sortBy (\k -> ( b2ClassRank k, barycenter funcG aRanks1 k ))
    in
    { aOrder = aOrder1, b1Order = b1Order1, b2Order = b2Order1 }


{-| Compute structured positions for A, B1, B2 nodes based on equivalence class orderings.
A nodes along top-left → bottom-right diagonal.
B1 nodes along top-right (outer) → bottom-left (center) anti-diagonal.
B2 nodes along bottom-left → top-right diagonal (anti-diagonal).
-}
computeStructuredPositions : { aOrder : List Int, b1Order : List Int, b2Order : List Int } -> NodePositions
computeStructuredPositions orderings =
    let
        along group diagonal order =
            List.indexedMap
                (\rank i -> ( nodeId group i, pointAlong diagonal (spreadT (List.length order) rank) ))
                order
    in
    Dict.fromList
        (along GroupA diagonalA orderings.aOrder
            ++ along GroupB1 diagonalB1 orderings.b1Order
            ++ along GroupB2 diagonalB2 orderings.b2Order
        )


{-| Everything the entity and force builders need to know about the current
configuration, computed once per (re)layout.
-}
type alias Layout =
    { viewMode : ViewMode
    , sizeA : Int
    , sizeB1 : Int
    , sizeB2 : Int
    , funcF : List Int
    , funcG : List Int
    , equivClasses : List (List String)

    -- Four-quadrant diagonal positions of A, B1 and B2 nodes
    , structuredPos : NodePositions

    -- Each equivalence class with the center of its blob
    , blobs : List ( List String, Point )

    -- Center of the blob every node (including A and B nodes) ends up in
    , blobCenterOf : NodePositions
    }


computeLayout :
    ViewMode
    -> { a | sizeA : Int, sizeB1 : Int, sizeB2 : Int, funcF : List Int, funcG : List Int, equivClasses : List (List String) }
    -> Layout
computeLayout viewMode config =
    let
        blobs =
            computeBlobCenters viewMode config.equivClasses

        pbCenterOf =
            blobs
                |> List.concatMap (\( cls, center ) -> List.map (\id -> ( id, center )) cls)
                |> Dict.fromList

        -- A node i lands in the blob of pb1_{f(i)} (which is also the blob of pb2_{g(i)})
        aCenters =
            List.indexedMap (\i fi -> ( nodeId GroupA i, nodeId GroupPB1 fi )) config.funcF

        injectedCenters group size =
            List.range 0 (size - 1)
                |> List.filterMap
                    (\i ->
                        injectionTarget group
                            |> Maybe.map (\pbGroup -> ( nodeId group i, nodeId pbGroup i ))
                    )

        derivedCenterOf =
            (aCenters ++ injectedCenters GroupB1 config.sizeB1 ++ injectedCenters GroupB2 config.sizeB2)
                |> List.filterMap (\( id, pbId ) -> Dict.get pbId pbCenterOf |> Maybe.map (Tuple.pair id))
                |> Dict.fromList
    in
    { viewMode = viewMode
    , sizeA = config.sizeA
    , sizeB1 = config.sizeB1
    , sizeB2 = config.sizeB2
    , funcF = config.funcF
    , funcG = config.funcG
    , equivClasses = config.equivClasses
    , structuredPos =
        computeStructuredPositions
            (computeEquivClassOrderings config.sizeA config.sizeB1 config.sizeB2 config.funcF config.funcG config.equivClasses)
    , blobs = blobs
    , blobCenterOf = Dict.union pbCenterOf derivedCenterOf
    }


makeEntity : EntityGroup -> Int -> Point -> ForceEntity
makeEntity group i pos =
    { x = pos.x
    , y = pos.y
    , vx = 0
    , vy = 0
    , id = nodeId group i
    , value = { group = group, index = i, label = groupLabel group i }
    }


buildEntities : Layout -> List ForceEntity
buildEntities layout =
    let
        entitiesAt group size positions default =
            List.map
                (\i ->
                    makeEntity group i (Dict.get (nodeId group i) positions |> Maybe.withDefault default)
                )
                (List.range 0 (size - 1))

        -- Pushout nodes start on a small golden-angle spiral around their blob center
        spiralPositions =
            List.concatMap
                (\( cls, center ) ->
                    let
                        n =
                            List.length cls
                    in
                    List.indexedMap
                        (\mi memberId ->
                            let
                                angle =
                                    toFloat mi * goldenAngle

                                r =
                                    12 * sqrt (toFloat (mi + 1) / toFloat (max n 1))
                            in
                            ( memberId, { x = center.x + r * cos angle, y = center.y + r * sin angle } )
                        )
                        cls
                )
                layout.blobs
                |> Dict.fromList
    in
    entitiesAt GroupA layout.sizeA layout.structuredPos quadA
        ++ entitiesAt GroupB1 layout.sizeB1 layout.structuredPos quadB1
        ++ entitiesAt GroupB2 layout.sizeB2 layout.structuredPos quadB2
        ++ entitiesAt GroupPB1 layout.sizeB1 spiralPositions quadP
        ++ entitiesAt GroupPB2 layout.sizeB2 spiralPositions quadP


{-| Sort classes by descending size, ties broken lexicographically. Members are sorted too.
-}
sortClassesBySize : List (List String) -> List (List String)
sortClassesBySize classes =
    classes
        |> List.map List.sort
        |> List.sortWith
            (\a b ->
                case compare (List.length b) (List.length a) of
                    EQ ->
                        compare a b

                    ord ->
                        ord
            )


computeBlobCenters : ViewMode -> List (List String) -> List ( List String, Point )
computeBlobCenters viewMode classes =
    let
        n =
            List.length classes
    in
    case viewMode of
        FourQuadrant ->
            -- Arrange blobs along bottom-right (outer) → top-left (center) diagonal,
            -- in the same order as computeEquivClassOrderings
            sortClassesBySize classes
                |> List.indexedMap (\i cls -> ( cls, pointAlong diagonalP (spreadT n i) ))

        ZoomedPushout ->
            let
                radius =
                    if n <= 1 then
                        0

                    else
                        min 220 (60 * sqrt (toFloat n))
            in
            classes
                |> List.indexedMap
                    (\i cls ->
                        let
                            angle =
                                toFloat i * 2 * pi / toFloat (max n 1)
                        in
                        ( cls
                        , { x = canvasCenter.x + radius * cos angle
                          , y = canvasCenter.y + radius * sin angle
                          }
                        )
                    )


{-| Centre-to-centre distance the collision force keeps between nodes.
Links use the same distance, so links and collisions agree on where nodes
should rest instead of pulling against each other.
-}
nodeSpacing : Float
nodeSpacing =
    40


{-| Link the members of a class into a ring, so that (unlike in a star)
every member carries the same number of links.
-}
ringLinks : List String -> List ( String, String )
ringLinks members =
    case members of
        [] ->
            []

        [ _ ] ->
            []

        [ a, b ] ->
            [ ( a, b ) ]

        first :: rest ->
            List.map2 Tuple.pair members (rest ++ [ first ])


buildForces : Layout -> List ForceEntity -> List (Force.Force String)
buildForces layout entities =
    let
        allIds =
            List.map .id entities

        ( positionStrength, manyBodyStrength ) =
            case layout.viewMode of
                FourQuadrant ->
                    ( 0.3, -10 )

                ZoomedPushout ->
                    ( 0.03, -40 )

        -- Leaving strength to the library scales it down for nodes with many links
        link ( source, target ) =
            { source = source, target = target, distance = nodeSpacing, strength = Nothing }

        blobLinks =
            List.concatMap ringLinks layout.equivClasses

        -- In the zoomed view, link A nodes to their images so they're pulled into blobs
        aToBlobLinks =
            case layout.viewMode of
                FourQuadrant ->
                    []

                ZoomedPushout ->
                    List.indexedMap (\i fi -> ( nodeId GroupA i, nodeId GroupPB1 fi )) layout.funcF
                        ++ List.indexedMap (\i gi -> ( nodeId GroupA i, nodeId GroupPB2 gi )) layout.funcG

        posTargets =
            List.map (positionTarget layout positionStrength) entities
    in
    [ Force.towardsX (List.map (\t -> { node = t.node, strength = t.strength, target = t.target.x }) posTargets)
    , Force.towardsY (List.map (\t -> { node = t.node, strength = t.strength, target = t.target.y }) posTargets)
    , Force.customCollision { iterations = 1, strength = 0.7 } (List.map (\id -> ( id, nodeSpacing / 2 )) allIds)
    , Force.manyBodyStrength manyBodyStrength allIds
    , Force.customLinks 1 (List.map link (blobLinks ++ aToBlobLinks))
    ]


positionTarget : Layout -> Float -> ForceEntity -> { node : String, strength : Float, target : Point }
positionTarget layout strength ent =
    let
        towards factor positions default =
            { node = ent.id
            , strength = strength * factor
            , target = Dict.get ent.id positions |> Maybe.withDefault default
            }

        towardsBlob factor =
            towards factor layout.blobCenterOf canvasCenter
    in
    case ( layout.viewMode, ent.value.group ) of
        ( FourQuadrant, GroupA ) ->
            towards 1 layout.structuredPos quadA

        ( FourQuadrant, GroupB1 ) ->
            towards 1 layout.structuredPos quadB1

        ( FourQuadrant, GroupB2 ) ->
            towards 1 layout.structuredPos quadB2

        ( ZoomedPushout, GroupA ) ->
            towardsBlob 3

        ( ZoomedPushout, GroupB1 ) ->
            towardsBlob 1.5

        ( ZoomedPushout, GroupB2 ) ->
            towardsBlob 1.5

        ( _, GroupPB1 ) ->
            towardsBlob 1

        ( _, GroupPB2 ) ->
            towardsBlob 1


rebuildSimulation : ViewMode -> Model -> Model
rebuildSimulation viewMode model =
    let
        equivClasses =
            computeEquivClasses model.sizeB1 model.sizeB2 model.funcF model.funcG

        layout =
            computeLayout viewMode { model | equivClasses = equivClasses }

        entities =
            ensureEntities layout model.entities
    in
    { model
        | equivClasses = equivClasses
        , entities = entities
        , forceState = Force.simulation (buildForces layout entities) |> Force.iterations 800
        , currentView = viewMode
    }


transitionToView : ViewMode -> Model -> Model
transitionToView viewMode model =
    let
        equivClasses =
            computeEquivClasses model.sizeB1 model.sizeB2 model.funcF model.funcG

        layout =
            computeLayout viewMode { model | equivClasses = equivClasses }

        -- Build forces for the new view but keep current entity positions
        newEntities =
            ensureEntities layout model.entities

        -- Run simulation to completion to get final target positions
        targetEntities =
            runSimulationToCompletion
                (Force.simulation (buildForces layout newEntities) |> Force.iterations 800)
                newEntities

        startPositions =
            entitiesToPositions newEntities

        finalPositions =
            entitiesToPositions targetEntities

        midPositions =
            computeMidPositions viewMode newEntities startPositions finalPositions

        phase1 =
            case viewMode of
                ZoomedPushout ->
                    MergePhase { nextTargets = finalPositions }

                FourQuadrant ->
                    RearrangePhase { nextTargets = finalPositions }
    in
    { model
        | equivClasses = equivClasses
        , entities = newEntities
        , forceState = Force.simulation []
        , currentView = viewMode
        , transition =
            Just
                { startPositions = startPositions
                , targetPositions = midPositions
                , progress = 0
                , phase = phase1
                }
    }


entitiesToPositions : List ForceEntity -> NodePositions
entitiesToPositions entities =
    List.foldl (\e -> Dict.insert e.id { x = e.x, y = e.y }) Dict.empty entities


{-| Positions halfway through a view transition, where every B node sits on top
of the pushout node it is injected into.

The transition always passes through the four-quadrant layout with B nodes merged:

  - FQ → ZP: first merge (only B nodes move, onto their current pb positions),
    then rearrange everything into the zoomed layout.
  - ZP → FQ: first rearrange everything into the final FQ layout with B nodes still
    on their pb positions, then split the B nodes off.

-}
computeMidPositions : ViewMode -> List ForceEntity -> NodePositions -> NodePositions -> NodePositions
computeMidPositions targetView entities startPositions finalPositions =
    let
        basePositions =
            case targetView of
                ZoomedPushout ->
                    startPositions

                FourQuadrant ->
                    finalPositions

        mergedPositions =
            List.filterMap
                (\e ->
                    injectionTarget e.value.group
                        |> Maybe.andThen (\pbGroup -> Dict.get (nodeId pbGroup e.value.index) basePositions)
                        |> Maybe.map (Tuple.pair e.id)
                )
                entities
    in
    Dict.union (Dict.fromList mergedPositions) basePositions


{-| Run the force simulation until it completes, returning final entity positions.
-}
runSimulationToCompletion : Force.State String -> List ForceEntity -> List ForceEntity
runSimulationToCompletion state entities =
    if Force.isCompleted state then
        entities

    else
        let
            ( newState, newEntities ) =
                Force.tick state entities
        in
        runSimulationToCompletion newState newEntities


{-| A simulation with the given forces whose alpha has already decayed to its minimum.
Force.State is opaque, so decay alpha by ticking it on an empty entity list.
-}
settledSimulation : List (Force.Force String) -> Force.State String
settledSimulation forces =
    let
        decay state =
            if Force.isCompleted state then
                state

            else
                decay (Tuple.first (Force.tick state []))
    in
    decay (Force.simulation forces)


{-| The given simulation reheated to a moderate alpha of about 0.3, so that dragging
a node nudges its neighbours instead of shaking the whole layout as Force.reheat
(alpha 1) would. Force.State is opaque, so count how many ticks a full cool-down
takes, then decay the reheated state by the share of them that brings alpha
from 1 down to 0.3 (alpha decays geometrically to its minimum of 0.001).
-}
warmSimulation : Force.State String -> Force.State String
warmSimulation state =
    let
        cool s =
            Tuple.first (Force.tick s [])

        ticksToSettle s n =
            if Force.isCompleted s then
                n

            else
                ticksToSettle (cool s) (n + 1)

        reheated =
            Force.reheat state

        warmTicks =
            round (toFloat (ticksToSettle reheated 0) * logBase 0.001 0.3)
    in
    List.foldl (\_ s -> cool s) reheated (List.range 1 warmTicks)


{-| Ensure all needed entities exist, preserving positions of existing ones.
New entities get default positions. Velocities are reset, so motion left over
from the previous layout doesn't carry into the new one.
-}
ensureEntities : Layout -> List ForceEntity -> List ForceEntity
ensureEntities layout oldEntities =
    let
        oldDict =
            List.foldl (\e d -> Dict.insert e.id e d) Dict.empty oldEntities
    in
    List.map
        (\fresh ->
            case Dict.get fresh.id oldDict of
                Just old ->
                    { old | value = fresh.value, vx = 0, vy = 0 }

                Nothing ->
                    fresh
        )
        (buildEntities layout)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            case model.transition of
                Just t ->
                    let
                        -- Each phase takes ~1500ms (3s total for two phases)
                        newProgress =
                            min 1.0 (t.progress + dt / 1500)

                        eased =
                            smoothStep newProgress

                        interpolatedEntities =
                            List.map
                                (\e ->
                                    let
                                        start =
                                            Dict.get e.id t.startPositions
                                                |> Maybe.withDefault { x = e.x, y = e.y }

                                        target =
                                            Dict.get e.id t.targetPositions
                                                |> Maybe.withDefault { x = e.x, y = e.y }
                                    in
                                    { e
                                        | x = start.x + (target.x - start.x) * eased
                                        , y = start.y + (target.y - start.y) * eased
                                    }
                                )
                                model.entities
                    in
                    if newProgress >= 1.0 then
                        case phaseNextTargets t.phase of
                            Just nextTargets ->
                                ( { model
                                    | entities = interpolatedEntities
                                    , transition =
                                        Just
                                            { startPositions = t.targetPositions
                                            , targetPositions = nextTargets
                                            , progress = 0
                                            , phase = FinalPhase
                                            }
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                -- FinalPhase: entities now sit at the precomputed equilibrium.
                                -- Restarting the simulation at full alpha would only shake them
                                -- around, so start it already settled (drag will reheat it).
                                let
                                    forces =
                                        buildForces (computeLayout model.currentView model) interpolatedEntities
                                in
                                ( { model
                                    | entities = List.map (\e -> { e | vx = 0, vy = 0 }) interpolatedEntities
                                    , transition = Nothing
                                    , forceState = settledSimulation forces
                                  }
                                , Cmd.none
                                )

                    else
                        ( { model
                            | entities = interpolatedEntities
                            , transition = Just { t | progress = newProgress }
                          }
                        , Cmd.none
                        )

                Nothing ->
                    if model.simulationPaused || Force.isCompleted model.forceState then
                        ( model, Cmd.none )

                    else
                        let
                            ( newState, tickedEntities ) =
                                Force.tick model.forceState model.entities

                            -- Keep a dragged node pinned under the cursor; only the other nodes react to forces
                            newEntities =
                                case model.drag of
                                    Just (DraggingNode ds) ->
                                        List.map2
                                            (\old new ->
                                                if new.id == ds.entityId then
                                                    { new | x = old.x, y = old.y, vx = 0, vy = 0 }

                                                else
                                                    new
                                            )
                                            model.entities
                                            tickedEntities

                                    _ ->
                                        tickedEntities
                        in
                        ( { model | forceState = newState, entities = newEntities }, Cmd.none )

        SetSize which str ->
            let
                fallback =
                    case which of
                        SetA ->
                            model.sizeA

                        SetB1 ->
                            model.sizeB1

                        SetB2 ->
                            model.sizeB2

                size =
                    String.toInt str |> Maybe.withDefault fallback

                newModel =
                    case which of
                        SetA ->
                            { model | sizeA = size }

                        SetB1 ->
                            { model | sizeB1 = size }

                        SetB2 ->
                            { model | sizeB2 = size }
            in
            ( newModel
            , Random.generate GotRandomFunctions (randomFunctionPair newModel.sizeA newModel.sizeB1 newModel.sizeB2)
            )

        GotRandomFunctions ( f, g ) ->
            let
                newModel =
                    rebuildSimulation model.currentView { model | funcF = f, funcG = g }
            in
            ( newModel, Cmd.none )

        RandomizeFunctions ->
            ( model
            , Random.generate GotRandomFunctions (randomFunctionPair model.sizeA model.sizeB1 model.sizeB2)
            )

        RandomizeSingle which kind ->
            let
                sizeCod =
                    case which of
                        FuncF ->
                            model.sizeB1

                        FuncG ->
                            model.sizeB2
            in
            ( model
            , Random.generate (GotRandomSingle which) (randomFunction kind model.sizeA sizeCod)
            )

        GotRandomSingle which vals ->
            let
                updated =
                    case which of
                        FuncF ->
                            { model | funcF = vals }

                        FuncG ->
                            { model | funcG = vals }
            in
            ( rebuildSimulation model.currentView updated, Cmd.none )

        SwitchView viewMode ->
            if viewMode == model.currentView then
                ( model, Cmd.none )

            else
                ( transitionToView viewMode model, Cmd.none )

        ArrowDragStart func aIndex pos ->
            ( { model | drag = Just (DraggingArrow { func = func, aIndex = aIndex, mousePos = pos }) }
            , Cmd.none
            )

        NodeDragStart entityId clientPos ->
            let
                ( svgX, svgY ) =
                    clientToSvg model clientPos

                -- Compute offset between mouse and entity center so drag feels natural
                maybeEnt =
                    List.filter (\e -> e.id == entityId) model.entities |> List.head

                offsetXY =
                    case maybeEnt of
                        Just ent ->
                            ( svgX - ent.x, svgY - ent.y )

                        Nothing ->
                            ( 0, 0 )
            in
            ( { model
                | drag =
                    Just
                        (DraggingNode
                            { entityId = entityId
                            , offset = offsetXY
                            , warmState = warmSimulation model.forceState
                            }
                        )
              }
            , Cmd.none
            )

        MouseMove clientPos ->
            case model.drag of
                Just (DraggingArrow ds) ->
                    ( { model | drag = Just (DraggingArrow { ds | mousePos = clientPos }) }, Cmd.none )

                Just (DraggingNode ds) ->
                    let
                        ( rawSvgX, rawSvgY ) =
                            clientToSvg model clientPos

                        svgX =
                            rawSvgX - Tuple.first ds.offset

                        svgY =
                            rawSvgY - Tuple.second ds.offset

                        -- Find the entity to get its group for clamping
                        maybeEnt =
                            List.filter (\e -> e.id == ds.entityId) model.entities |> List.head

                        ( clampedX, clampedY ) =
                            case maybeEnt of
                                Just ent ->
                                    clampToSet model.currentView ent.value.group svgX svgY

                                Nothing ->
                                    ( svgX, svgY )

                        newEntities =
                            List.map
                                (\e ->
                                    if e.id == ds.entityId then
                                        { e | x = clampedX, y = clampedY, vx = 0, vy = 0 }

                                    else
                                        e
                                )
                                model.entities
                    in
                    ( { model
                        | entities = newEntities
                        , forceState = ds.warmState
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MouseUp clientPos ->
            case model.drag of
                Just (DraggingArrow ds) ->
                    let
                        ( svgX, svgY ) =
                            clientToSvg model clientPos

                        -- Find nearest valid target
                        validTargets =
                            List.filter (\e -> e.value.group == dropTargetGroup model.currentView ds.func) model.entities

                        nearest =
                            validTargets
                                |> List.map (\e -> ( sqrt ((e.x - svgX) ^ 2 + (e.y - svgY) ^ 2), e ))
                                |> List.sortBy Tuple.first
                                |> List.head

                        updatedModel =
                            case nearest of
                                Just ( dist, target ) ->
                                    if dist < 30 then
                                        let
                                            targetIdx =
                                                String.split "_" target.id
                                                    |> List.drop 1
                                                    |> List.head
                                                    |> Maybe.andThen String.toInt
                                                    |> Maybe.withDefault 0
                                        in
                                        updateMapping ds.func ds.aIndex targetIdx model

                                    else
                                        model

                                Nothing ->
                                    model
                    in
                    ( { updatedModel | drag = Nothing }, Cmd.none )

                Just (DraggingNode _) ->
                    ( { model | drag = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotSvgElement result ->
            case result of
                Ok { element } ->
                    ( { model
                        | svgOffset =
                            { x = element.x
                            , y = element.y
                            , width = element.width
                            , height = element.height
                            }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ToggleSimulationPaused paused ->
            ( { model | simulationPaused = paused }, Cmd.none )

        SetColorMode mode ->
            ( { model | colorMode = mode }, Cmd.none )

        WindowResized ->
            ( model, Browser.Dom.getElement svgId |> Task.attempt GotSvgElement )


updateMapping : WhichFunc -> Int -> Int -> Model -> Model
updateMapping func aIndex targetIdx model =
    let
        replaceAt i newVal list =
            List.indexedMap
                (\idx val ->
                    if idx == i then
                        newVal

                    else
                        val
                )
                list

        newModel =
            case func of
                FuncF ->
                    { model | funcF = replaceAt aIndex targetIdx model.funcF }

                FuncG ->
                    { model | funcG = replaceAt aIndex targetIdx model.funcG }
    in
    rebuildSimulation newModel.currentView newModel



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animSub =
            if model.transition /= Nothing || (not model.simulationPaused && not (Force.isCompleted model.forceState)) then
                Browser.Events.onAnimationFrameDelta Tick

            else
                Sub.none

        dragSubs =
            case model.drag of
                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove
                            (Decode.map2 (\x y -> MouseMove ( x, y ))
                                (Decode.field "clientX" Decode.float)
                                (Decode.field "clientY" Decode.float)
                            )
                        , Browser.Events.onMouseUp
                            (Decode.map2 (\x y -> MouseUp ( x, y ))
                                (Decode.field "clientX" Decode.float)
                                (Decode.field "clientY" Decode.float)
                            )
                        , Browser.Events.onAnimationFrameDelta Tick
                        ]

                Nothing ->
                    animSub
    in
    Sub.batch
        [ dragSubs
        , Browser.Events.onResize (\_ _ -> WindowResized)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.style "font-family" "monospace"
        , Attr.style "display" "flex"
        , Attr.style "height" "100vh"
        , Attr.style "overflow" "hidden"
        ]
        [ viewControlsPanel model
        , Html.div
            [ Attr.style "flex" "1"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "stretch"
            ]
            [ viewCanvas model ]
        ]


viewControlsPanel : Model -> Html Msg
viewControlsPanel model =
    let
        transitioning =
            model.transition /= Nothing
    in
    Html.div
        [ Attr.style "width" "20%"
        , Attr.style "min-width" "180px"
        , Attr.style "max-width" "250px"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "12px"
        , Attr.style "padding" "12px"
        , Attr.style "background" "#f5f5f5"
        , Attr.style "border-right" "1px solid #ddd"
        , Attr.style "overflow-y" "auto"
        ]
        [ viewSlider "|A|" model.sizeA (SetSize SetA) colorA
        , viewSlider "|B1|" model.sizeB1 (SetSize SetB1) colorB1
        , viewSlider "|B2|" model.sizeB2 (SetSize SetB2) colorB2
        , Html.div []
            [ Html.span [ Attr.style "font-size" "12px", Attr.style "display" "block", Attr.style "margin-bottom" "4px" ] [ Html.text "View" ]
            , Html.div [ Attr.style "display" "flex" ]
                [ viewToggleButton "Four sets" FourQuadrant model.currentView transitioning "4px 0 0 4px"
                , viewToggleButton "Zoomed Pushout" ZoomedPushout model.currentView transitioning "0 4px 4px 0"
                ]
            ]
        , Html.div []
            [ Html.span [ Attr.style "font-size" "12px", Attr.style "display" "block", Attr.style "margin-bottom" "4px" ] [ Html.text "Force directed layout" ]
            , Html.div [ Attr.style "display" "flex" ]
                [ viewOnOffButton "Enabled" False model.simulationPaused "4px 0 0 4px"
                , viewOnOffButton "Disabled" True model.simulationPaused "0 4px 4px 0"
                ]
            ]
        , Html.div []
            [ Html.span [ Attr.style "font-size" "12px", Attr.style "display" "block", Attr.style "margin-bottom" "4px" ] [ Html.text "Color by" ]
            , Html.div [ Attr.style "display" "flex" ]
                [ viewColorModeButton "Set" ColorBySet model.colorMode "4px 0 0 4px"
                , viewColorModeButton "Equiv class" ColorByEquivClass model.colorMode "0 4px 4px 0"
                ]
            ]
        , viewRandomizeSection model transitioning
        ]


viewRandomizeSection : Model -> Bool -> Html Msg
viewRandomizeSection model transitioning =
    let
        fCod =
            model.sizeB1

        gCod =
            model.sizeB2

        header label =
            Html.div
                [ Attr.style "font-size" "11px"
                , Attr.style "text-align" "center"
                , Attr.style "color" "#666"
                ]
                [ Html.text label ]

        rowLabel label =
            Html.div
                [ Attr.style "font-family" "monospace"
                , Attr.style "font-size" "12px"
                , Attr.style "font-weight" "bold"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "flex-end"
                , Attr.style "padding-right" "6px"
                ]
                [ Html.text label ]

        btn label msg disabled =
            Html.button
                [ Html.Events.onClick msg
                , Attr.disabled disabled
                , Attr.style "padding" "5px 4px"
                , Attr.style "font-family" "monospace"
                , Attr.style "font-size" "11px"
                , Attr.style "cursor"
                    (if disabled then
                        "not-allowed"

                     else
                        "pointer"
                    )
                , Attr.style "border" "1px solid #ccc"
                , Attr.style "border-radius" "4px"
                , Attr.style "background" "#fff"
                , Attr.style "width" "100%"
                , Attr.style "opacity"
                    (if disabled then
                        "0.5"

                     else
                        "1"
                    )
                ]
                [ Html.text label ]

        dis which kind =
            transitioning
                || not
                    (canGenerate kind
                        model.sizeA
                        (case which of
                            FuncF ->
                                fCod

                            FuncG ->
                                gCod
                        )
                    )

        bothPairDisabled =
            transitioning
                || not (canGenerate GenAny model.sizeA fCod && canGenerate GenAny model.sizeA gCod)
    in
    Html.div []
        [ Html.span [ Attr.style "font-size" "12px", Attr.style "display" "block", Attr.style "margin-bottom" "4px" ] [ Html.text "Randomize functions" ]
        , Html.div
            [ Attr.style "display" "grid"
            , Attr.style "grid-template-columns" "auto 1fr 1fr 1fr"
            , Attr.style "gap" "4px"
            , Attr.style "align-items" "stretch"
            ]
            [ Html.div [] []
            , header "Random"
            , header "Injective"
            , header "Surjective"
            , rowLabel "f"
            , btn "Random" (RandomizeSingle FuncF GenAny) (dis FuncF GenAny)
            , btn "Injective" (RandomizeSingle FuncF GenInjective) (dis FuncF GenInjective)
            , btn "Surjective" (RandomizeSingle FuncF GenSurjective) (dis FuncF GenSurjective)
            , rowLabel "g"
            , btn "Random" (RandomizeSingle FuncG GenAny) (dis FuncG GenAny)
            , btn "Injective" (RandomizeSingle FuncG GenInjective) (dis FuncG GenInjective)
            , btn "Surjective" (RandomizeSingle FuncG GenSurjective) (dis FuncG GenSurjective)
            , rowLabel "f, g"
            , btn "Random" RandomizeFunctions bothPairDisabled
            , Html.div [] []
            , Html.div [] []
            ]
        ]


viewSlider : String -> Int -> (String -> Msg) -> String -> Html Msg
viewSlider label val toMsg color =
    Html.div []
        [ Html.span [ Attr.style "color" color, Attr.style "font-weight" "bold", Attr.style "font-size" "12px" ]
            [ Html.text (label ++ " = " ++ String.fromInt val) ]
        , Html.input
            [ Attr.type_ "range"
            , Attr.min "0"
            , Attr.max "10"
            , Attr.value (String.fromInt val)
            , Html.Events.onInput toMsg
            , Attr.style "width" "100%"
            ]
            []
        ]


viewToggleButton : String -> ViewMode -> ViewMode -> Bool -> String -> Html Msg
viewToggleButton label targetView currentView transitioning borderRadius =
    Html.button
        [ Html.Events.onClick (SwitchView targetView)
        , Attr.disabled transitioning
        , Attr.style "padding" "5px 12px"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "cursor"
            (if transitioning then
                "not-allowed"

             else
                "pointer"
            )
        , Attr.style "background"
            (if targetView == currentView then
                "#ddeeff"

             else
                "#fff"
            )
        , Attr.style "border"
            (if targetView == currentView then
                "2px solid #4488cc"

             else
                "1px solid #ccc"
            )
        , Attr.style "border-radius" borderRadius
        , Attr.style "margin-left" "-1px"
        , Attr.style "opacity"
            (if transitioning then
                "0.5"

             else
                "1"
            )
        ]
        [ Html.text label ]


viewOnOffButton : String -> Bool -> Bool -> String -> Html Msg
viewOnOffButton label targetPaused currentPaused borderRadius =
    let
        isActive =
            targetPaused == currentPaused
    in
    Html.button
        [ Html.Events.onClick (ToggleSimulationPaused targetPaused)
        , Attr.style "padding" "5px 12px"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "cursor" "pointer"
        , Attr.style "background"
            (if isActive then
                "#ddeeff"

             else
                "#fff"
            )
        , Attr.style "border"
            (if isActive then
                "2px solid #4488cc"

             else
                "1px solid #ccc"
            )
        , Attr.style "border-radius" borderRadius
        , Attr.style "margin-left" "-1px"
        ]
        [ Html.text label ]


viewColorModeButton : String -> ColorMode -> ColorMode -> String -> Html Msg
viewColorModeButton label targetMode currentMode borderRadius =
    let
        isActive =
            targetMode == currentMode
    in
    Html.button
        [ Html.Events.onClick (SetColorMode targetMode)
        , Attr.style "padding" "5px 12px"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "cursor" "pointer"
        , Attr.style "background"
            (if isActive then
                "#ddeeff"

             else
                "#fff"
            )
        , Attr.style "border"
            (if isActive then
                "2px solid #4488cc"

             else
                "1px solid #ccc"
            )
        , Attr.style "border-radius" borderRadius
        , Attr.style "margin-left" "-1px"
        ]
        [ Html.text label ]


svgId : String
svgId =
    "pushout-svg"


viewCanvas : Model -> Html Msg
viewCanvas model =
    Svg.svg
        [ SA.id svgId
        , SA.viewBox ("0 0 " ++ String.fromFloat canvasW ++ " " ++ String.fromFloat canvasH)
        , Attr.style "background" "#fafafa"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "display" "block"
        ]
        (svgDefs
            :: viewQuadrantLines model.currentView
            ++ viewQuadrantLabels model.currentView
            ++ viewBlobs model
            ++ viewArrows model
            ++ viewEntitiesLayer model
            ++ viewDragArrow model
        )


svgDefs : Svg Msg
svgDefs =
    let
        arrowMarker id color w h =
            Svg.node "marker"
                [ SA.id id
                , SA.viewBox "0 0 10 6"
                , SA.refX "10"
                , SA.refY "3"
                , SA.markerWidth (String.fromFloat w)
                , SA.markerHeight (String.fromFloat h)
                , SA.orient "auto"
                ]
                [ Svg.polygon [ SA.points "0,0 10,3 0,6", SA.fill color ] [] ]
    in
    Svg.defs []
        [ arrowMarker "arrowF" colorB1 8 6
        , arrowMarker "arrowG" colorB2 8 6
        , arrowMarker "arrowInject" "#999" 6 4
        , arrowMarker "arrowDrag" "#ff8800" 8 6
        ]



-- QUADRANT LINES & LABELS


viewQuadrantLines : ViewMode -> List (Svg Msg)
viewQuadrantLines viewMode =
    case viewMode of
        FourQuadrant ->
            [ Svg.line
                [ SA.x1 (String.fromFloat (canvasW / 2))
                , SA.y1 "0"
                , SA.x2 (String.fromFloat (canvasW / 2))
                , SA.y2 (String.fromFloat canvasH)
                , SA.stroke "#ddd"
                , SA.strokeDasharray "6"
                ]
                []
            , Svg.line
                [ SA.x1 "0"
                , SA.y1 (String.fromFloat (canvasH / 2))
                , SA.x2 (String.fromFloat canvasW)
                , SA.y2 (String.fromFloat (canvasH / 2))
                , SA.stroke "#ddd"
                , SA.strokeDasharray "6"
                ]
                []
            ]

        ZoomedPushout ->
            []


viewQuadrantLabels : ViewMode -> List (Svg Msg)
viewQuadrantLabels viewMode =
    case viewMode of
        FourQuadrant ->
            [ svgLabel 30 25 colorA "A"
            , svgLabel 430 25 colorB1 "B1"
            , svgLabel 30 325 colorB2 "B2"
            , svgLabel 430 325 "#888" "P = pushout"
            ]

        ZoomedPushout ->
            [ svgLabel 30 25 "#888" "P = pushout (zoomed)" ]


svgLabel : Float -> Float -> String -> String -> Svg Msg
svgLabel x y color text =
    Svg.text_
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat y)
        , SA.fontSize "14"
        , SA.fontFamily "monospace"
        , SA.fontWeight "bold"
        , SA.fill color
        , SA.style "user-select: none"
        ]
        [ Svg.text text ]



-- BLOB RENDERING


viewBlobs : Model -> List (Svg Msg)
viewBlobs model =
    let
        entDict =
            entityDict model.entities
    in
    List.filterMap (viewBlob entDict) model.equivClasses


viewBlob : Dict String ForceEntity -> List String -> Maybe (Svg Msg)
viewBlob entDict cls =
    let
        members =
            List.filterMap (\id -> Dict.get id entDict) cls
    in
    if List.isEmpty members then
        Nothing

    else
        let
            xs =
                List.map .x members

            ys =
                List.map .y members

            cx =
                List.sum xs / toFloat (List.length xs)

            cy =
                List.sum ys / toFloat (List.length ys)

            maxDist =
                List.map (\e -> sqrt ((e.x - cx) ^ 2 + (e.y - cy) ^ 2)) members
                    |> List.maximum
                    |> Maybe.withDefault 0

            r =
                maxDist + 22
        in
        Just
            (Svg.circle
                [ SA.cx (String.fromFloat cx)
                , SA.cy (String.fromFloat cy)
                , SA.r (String.fromFloat r)
                , SA.fill "rgba(180,180,180,0.12)"
                , SA.stroke "#ccc"
                , SA.strokeDasharray "4"
                , SA.strokeWidth "1"
                ]
                []
            )



-- ARROW RENDERING


viewArrows : Model -> List (Svg Msg)
viewArrows model =
    let
        entDict =
            entityDict model.entities

        isDragging func aIdx =
            case model.drag of
                Just (DraggingArrow ds) ->
                    ds.func == func && ds.aIndex == aIdx

                _ ->
                    False

        b1b2Visible =
            showB1B2 model
    in
    if b1b2Visible then
        -- FQ-style: a->b1, a->b2, injection b1->pb1, b2->pb2
        List.indexedMap
            (\i fi ->
                if isDragging FuncF i then
                    Svg.g [] []

                else
                    viewArrow entDict (nodeId GroupA i) (nodeId GroupB1 fi) "arrowF" colorB1 0.6 False (Just ( FuncF, i ))
            )
            model.funcF
            ++ List.indexedMap
                (\i gi ->
                    if isDragging FuncG i then
                        Svg.g [] []

                    else
                        viewArrow entDict (nodeId GroupA i) (nodeId GroupB2 gi) "arrowG" colorB2 0.6 False (Just ( FuncG, i ))
                )
                model.funcG
            ++ List.concatMap
                (\j ->
                    [ viewArrow entDict (nodeId GroupB1 j) (nodeId GroupPB1 j) "arrowInject" "#999" 0.3 True Nothing ]
                )
                (List.range 0 (model.sizeB1 - 1))
            ++ List.concatMap
                (\k ->
                    [ viewArrow entDict (nodeId GroupB2 k) (nodeId GroupPB2 k) "arrowInject" "#999" 0.3 True Nothing ]
                )
                (List.range 0 (model.sizeB2 - 1))

    else
        -- ZP-style: a->pb1, a->pb2 (draggable)
        List.indexedMap
            (\i fi ->
                if isDragging FuncF i then
                    Svg.g [] []

                else
                    viewArrow entDict (nodeId GroupA i) (nodeId GroupPB1 fi) "arrowF" colorB1 0.5 False (Just ( FuncF, i ))
            )
            model.funcF
            ++ List.indexedMap
                (\i gi ->
                    if isDragging FuncG i then
                        Svg.g [] []

                    else
                        viewArrow entDict (nodeId GroupA i) (nodeId GroupPB2 gi) "arrowG" colorB2 0.5 False (Just ( FuncG, i ))
                )
                model.funcG


viewArrow : Dict String ForceEntity -> String -> String -> String -> String -> Float -> Bool -> Maybe ( WhichFunc, Int ) -> Svg Msg
viewArrow entDict srcId tgtId markerId color opacity isDashed maybeDraggable =
    case ( Dict.get srcId entDict, Dict.get tgtId entDict ) of
        ( Just src, Just tgt ) ->
            let
                dx =
                    tgt.x - src.x

                dy =
                    tgt.y - src.y

                dist =
                    sqrt (dx * dx + dy * dy)

                -- Shorten arrows so they don't overlap with circles
                nodeR =
                    10

                ratio =
                    if dist < 1 then
                        0

                    else
                        nodeR / dist

                sx =
                    src.x + dx * ratio

                sy =
                    src.y + dy * ratio

                tx =
                    tgt.x - dx * ratio

                ty =
                    tgt.y - dy * ratio

                pathD =
                    "M " ++ ff sx ++ " " ++ ff sy ++ " L " ++ ff tx ++ " " ++ ff ty

                baseAttrs =
                    [ SA.d pathD
                    , SA.stroke color
                    , SA.strokeWidth "1.5"
                    , SA.fill "none"
                    , SA.opacity (String.fromFloat opacity)
                    , SA.markerEnd ("url(#" ++ markerId ++ ")")
                    ]
                        ++ (if isDashed then
                                [ SA.strokeDasharray "4" ]

                            else
                                []
                           )

                -- Draggable hit area at arrowhead
                hitArea =
                    case maybeDraggable of
                        Just ( func, aIdx ) ->
                            [ Svg.circle
                                [ SA.cx (ff tx)
                                , SA.cy (ff ty)
                                , SA.r "15"
                                , SA.fill "transparent"
                                , SA.style "cursor: grab"
                                , Svg.Events.on "mousedown"
                                    (Decode.map2 (\x y -> ArrowDragStart func aIdx ( x, y ))
                                        (Decode.field "clientX" Decode.float)
                                        (Decode.field "clientY" Decode.float)
                                    )
                                ]
                                []
                            ]

                        Nothing ->
                            []
            in
            Svg.g [] (Svg.path baseAttrs [] :: hitArea)

        _ ->
            Svg.g [] []



-- DRAG ARROW RENDERING


viewDragArrow : Model -> List (Svg Msg)
viewDragArrow model =
    case model.drag of
        Just (DraggingArrow ds) ->
            let
                entDict =
                    entityDict model.entities

                srcId =
                    nodeId GroupA ds.aIndex

                ( svgX, svgY ) =
                    clientToSvg model ds.mousePos
            in
            case Dict.get srcId entDict of
                Just src ->
                    let
                        pathD =
                            "M " ++ ff src.x ++ " " ++ ff src.y ++ " L " ++ ff svgX ++ " " ++ ff svgY
                    in
                    Svg.path
                        [ SA.d pathD
                        , SA.stroke "#ff8800"
                        , SA.strokeWidth "2"
                        , SA.fill "none"
                        , SA.markerEnd "url(#arrowDrag)"
                        , SA.opacity "0.8"
                        ]
                        []
                        :: viewValidTargetHighlights model ds

                Nothing ->
                    []

        _ ->
            []


viewValidTargetHighlights : Model -> { func : WhichFunc, aIndex : Int, mousePos : ( Float, Float ) } -> List (Svg Msg)
viewValidTargetHighlights model ds =
    let
        targets =
            List.filter (\e -> e.value.group == dropTargetGroup model.currentView ds.func) model.entities
    in
    List.map
        (\e ->
            Svg.circle
                [ SA.cx (ff e.x)
                , SA.cy (ff e.y)
                , SA.r "16"
                , SA.fill "none"
                , SA.stroke "#ff8800"
                , SA.strokeWidth "2.5"
                , SA.strokeDasharray "4"
                , SA.opacity "0.8"
                ]
                []
        )
        targets



-- ENTITY RENDERING


viewEntitiesLayer : Model -> List (Svg Msg)
viewEntitiesLayer model =
    let
        b1b2Visible =
            showB1B2 model

        visible =
            if b1b2Visible then
                model.entities

            else
                List.filter
                    (\e ->
                        case e.value.group of
                            GroupB1 ->
                                False

                            GroupB2 ->
                                False

                            _ ->
                                True
                    )
                    model.entities
    in
    let
        colorFn =
            case model.colorMode of
                ColorBySet ->
                    \e -> groupColor e.value.group

                ColorByEquivClass ->
                    let
                        colorMap =
                            equivClassColorMap model.funcF model.equivClasses
                    in
                    \e ->
                        let
                            colorId =
                                injectionTarget e.value.group
                                    |> Maybe.map (\pbGroup -> nodeId pbGroup e.value.index)
                                    |> Maybe.withDefault e.id
                        in
                        Dict.get colorId colorMap |> Maybe.withDefault (groupColor e.value.group)
    in
    List.map (viewEntity colorFn) visible


viewEntity : (ForceEntity -> String) -> ForceEntity -> Svg Msg
viewEntity colorFn ent =
    let
        color =
            colorFn ent

        r =
            case ent.value.group of
                GroupPB1 ->
                    8

                GroupPB2 ->
                    8

                _ ->
                    10
    in
    Svg.g []
        [ Svg.circle
            [ SA.cx (ff ent.x)
            , SA.cy (ff ent.y)
            , SA.r (String.fromFloat r)
            , SA.fill color
            , SA.style "cursor: grab"
            , Svg.Events.on "mousedown"
                (Decode.map2 (\cx cy -> NodeDragStart ent.id ( cx, cy ))
                    (Decode.field "clientX" Decode.float)
                    (Decode.field "clientY" Decode.float)
                )
            , SA.opacity "0.75"
            , SA.stroke color
            , SA.strokeWidth "1.5"
            , SA.strokeOpacity "0.5"
            ]
            []
        , Svg.text_
            [ SA.x (ff ent.x)
            , SA.y (ff (ent.y + 3.5))
            , SA.fontSize "9"
            , SA.fontFamily "monospace"
            , SA.fill "#fff"
            , SA.textAnchor "middle"
            , SA.style "pointer-events: none; user-select: none"
            ]
            [ Svg.text ent.value.label ]
        ]


groupColor : EntityGroup -> String
groupColor group =
    case group of
        GroupA ->
            colorA

        GroupB1 ->
            colorB1

        GroupB2 ->
            colorB2

        GroupPB1 ->
            colorPB1

        GroupPB2 ->
            colorPB2


equivClassPalette : Array.Array String
equivClassPalette =
    Array.fromList
        [ "#e6194b"
        , "#3cb44b"
        , "#4363d8"
        , "#f58231"
        , "#911eb4"
        , "#42d4f4"
        , "#f032e6"
        , "#bfef45"
        , "#fabed4"
        , "#469990"
        , "#dcbeff"
        , "#9A6324"
        , "#800000"
        , "#aaffc3"
        , "#808000"
        , "#000075"
        , "#a9a9a9"
        , "#e6beff"
        , "#fffac8"
        , "#ffd8b1"
        ]


{-| Colors of pushout nodes and A nodes by equivalence class.
B nodes take the color of the pushout node they are injected into.
-}
equivClassColorMap : List Int -> List (List String) -> Dict String String
equivClassColorMap funcF equivClasses =
    let
        paletteSize =
            Array.length equivClassPalette

        classColor classIndex =
            Array.get (modBy paletteSize classIndex) equivClassPalette
                |> Maybe.withDefault "#888"

        -- Map each pb entity to its class index
        pbColors =
            List.indexedMap
                (\classIndex cls ->
                    List.map (\memberId -> ( memberId, classColor classIndex )) cls
                )
                equivClasses
                |> List.concat
                |> Dict.fromList

        -- Map a_i -> class of pb1_{f(i)}
        aColors =
            List.indexedMap
                (\i fi ->
                    Dict.get (nodeId GroupPB1 fi) pbColors
                        |> Maybe.map (Tuple.pair (nodeId GroupA i))
                )
                funcF
                |> List.filterMap identity
    in
    Dict.union pbColors (Dict.fromList aColors)



-- LEGEND


viewLegend : Html Msg
viewLegend =
    Html.div
        [ Attr.style "margin-top" "8px"
        , Attr.style "font-size" "12px"
        , Attr.style "color" "#666"
        , Attr.style "display" "flex"
        , Attr.style "gap" "16px"
        , Attr.style "flex-wrap" "wrap"
        ]
        [ legendItem colorA "A elements"
        , legendItem colorB1 "B1 elements"
        , legendItem colorB2 "B2 elements"
        , legendItem colorPB1 "B1 in P"
        , legendItem colorPB2 "B2 in P"
        , Html.span [] [ Html.text "| Drag arrowheads to edit f,g" ]
        ]


legendItem : String -> String -> Html Msg
legendItem color label =
    Html.span []
        [ Html.span
            [ Attr.style "display" "inline-block"
            , Attr.style "width" "10px"
            , Attr.style "height" "10px"
            , Attr.style "border-radius" "50%"
            , Attr.style "background" color
            , Attr.style "margin-right" "4px"
            , Attr.style "vertical-align" "middle"
            ]
            []
        , Html.text label
        ]



-- HELPERS


dropTargetGroup : ViewMode -> WhichFunc -> EntityGroup
dropTargetGroup viewMode func =
    case ( viewMode, func ) of
        ( FourQuadrant, FuncF ) ->
            GroupB1

        ( FourQuadrant, FuncG ) ->
            GroupB2

        ( ZoomedPushout, FuncF ) ->
            GroupPB1

        ( ZoomedPushout, FuncG ) ->
            GroupPB2


smoothStep : Float -> Float
smoothStep p =
    p * p * (3 - 2 * p)


{-| Should B1/B2 nodes and FQ-style arrows be shown?
They are visible in FourQuadrant view and during merge/split phases.
-}
showB1B2 : Model -> Bool
showB1B2 model =
    case model.transition of
        Nothing ->
            model.currentView == FourQuadrant

        Just t ->
            case t.phase of
                MergePhase _ ->
                    True

                RearrangePhase _ ->
                    False

                FinalPhase ->
                    -- Final phase: show B1/B2 if we're going TO FourQuadrant (splitting)
                    model.currentView == FourQuadrant


{-| Clamp a position to stay within the set's visual region.
-}
clampToSet : ViewMode -> EntityGroup -> Float -> Float -> ( Float, Float )
clampToSet viewMode group x y =
    let
        margin =
            15

        bounds =
            case viewMode of
                ZoomedPushout ->
                    { xMin = margin, xMax = canvasW - margin, yMin = margin, yMax = canvasH - margin }

                FourQuadrant ->
                    case group of
                        GroupA ->
                            { xMin = margin, xMax = canvasW / 2 - margin, yMin = margin, yMax = canvasH / 2 - margin }

                        GroupB1 ->
                            { xMin = canvasW / 2 + margin, xMax = canvasW - margin, yMin = margin, yMax = canvasH / 2 - margin }

                        GroupB2 ->
                            { xMin = margin, xMax = canvasW / 2 - margin, yMin = canvasH / 2 + margin, yMax = canvasH - margin }

                        GroupPB1 ->
                            { xMin = canvasW / 2 + margin, xMax = canvasW - margin, yMin = canvasH / 2 + margin, yMax = canvasH - margin }

                        GroupPB2 ->
                            { xMin = canvasW / 2 + margin, xMax = canvasW - margin, yMin = canvasH / 2 + margin, yMax = canvasH - margin }
    in
    ( clamp bounds.xMin bounds.xMax x, clamp bounds.yMin bounds.yMax y )


entityDict : List ForceEntity -> Dict String ForceEntity
entityDict entities =
    List.foldl (\entity -> Dict.insert entity.id entity) Dict.empty entities


lowercaseLetter : Int -> String
lowercaseLetter i =
    String.fromChar (Char.fromCode (97 + modBy 26 i))


uppercaseLetter : Int -> String
uppercaseLetter i =
    String.fromChar (Char.fromCode (65 + modBy 26 i))


ff : Float -> String
ff =
    String.fromFloat
