module Main exposing (..)

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



-- MODEL


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
    , svgOffset : { x : Float, y : Float }
    , transition : Maybe Transition
    }


type alias Transition =
    { startPositions : Dict String { x : Float, y : Float }
    , targetPositions : Dict String { x : Float, y : Float }
    , progress : Float -- 0.0 to 1.0
    , phase : TransitionPhase
    }


{-| During a merge phase, B1/B2 are visible and FQ-style arrows are shown.
During a rearrange phase, B1/B2 are hidden and ZP-style arrows are shown.
-}
type TransitionPhase
    = MergePhase { nextTargets : Dict String { x : Float, y : Float } }
    | RearrangePhase { nextTargets : Dict String { x : Float, y : Float } }
    | FinalPhase


phaseNextTargets : TransitionPhase -> Maybe (Dict String { x : Float, y : Float })
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
    , label : String
    }


type EntityGroup
    = GroupA
    | GroupB1
    | GroupB2
    | GroupPB1
    | GroupPB2


type ViewMode
    = FourQuadrant
    | ZoomedPushout


type DragState
    = DraggingArrow { func : WhichFunc, aIndex : Int, mousePos : ( Float, Float ) }
    | DraggingNode { entityId : String, offset : ( Float, Float ) }


type WhichFunc
    = FuncF
    | FuncG


type WhichSet
    = SetA
    | SetB1
    | SetB2



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
    | GotSvgElement (Result Browser.Dom.Error Browser.Dom.Element)



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
      , svgOffset = { x = 0, y = 0 }
      , transition = Nothing
      }
    , Cmd.batch
        [ Random.generate GotRandomFunctions (randomFunctionPair sizeA sizeB1 sizeB2)
        , Browser.Dom.getElement "pushout-svg" |> Task.attempt GotSvgElement
        ]
    )



-- RANDOM


randomFunctionPair : Int -> Int -> Int -> Random.Generator ( List Int, List Int )
randomFunctionPair sizeA sizeB1 sizeB2 =
    Random.map2 Tuple.pair
        (if sizeB1 > 0 && sizeA > 0 then
            Random.list sizeA (Random.int 0 (sizeB1 - 1))

         else
            Random.constant []
        )
        (if sizeB2 > 0 && sizeA > 0 then
            Random.list sizeA (Random.int 0 (sizeB2 - 1))

         else
            Random.constant []
        )



-- PUSHOUT


computeEquivClasses : Int -> Int -> List Int -> List Int -> List (List String)
computeEquivClasses sizeB1 sizeB2 funcF funcG =
    let
        -- Start with singleton classes for each pb1_j and pb2_k
        initial =
            List.map (\j -> [ "pb1_" ++ String.fromInt j ]) (List.range 0 (sizeB1 - 1))
                ++ List.map (\k -> [ "pb2_" ++ String.fromInt k ]) (List.range 0 (sizeB2 - 1))

        -- For each a_i, merge class of pb1_{f(i)} with class of pb2_{g(i)}
        identifications =
            List.map2
                (\fi gi ->
                    ( "pb1_" ++ String.fromInt fi
                    , "pb2_" ++ String.fromInt gi
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


makeEntity : String -> Float -> Float -> EntityGroup -> String -> ForceEntity
makeEntity id x y group label =
    { x = x, y = y, vx = 0, vy = 0, id = id, value = { group = group, label = label } }


buildEntities : Int -> Int -> Int -> List (List String) -> ViewMode -> List ForceEntity
buildEntities sizeA sizeB1 sizeB2 equivClasses viewMode =
    let
        spreadRadius =
            50

        spiralPos center i total =
            let
                angle =
                    toFloat i * 2.3998632

                -- golden angle
                r =
                    spreadRadius * sqrt (toFloat (i + 1) / toFloat (max total 1))
            in
            ( center.x + r * cos angle, center.y + r * sin angle )

        aEntities =
            List.map
                (\i ->
                    let
                        ( x, y ) =
                            spiralPos quadA i sizeA
                    in
                    makeEntity ("a_" ++ String.fromInt i) x y GroupA (String.fromInt (i + 1))
                )
                (List.range 0 (sizeA - 1))

        b1Entities =
            List.map
                (\j ->
                    let
                        ( x, y ) =
                            spiralPos quadB1 j sizeB1
                    in
                    makeEntity ("b1_" ++ String.fromInt j) x y GroupB1 (lowercaseLetter j)
                )
                (List.range 0 (sizeB1 - 1))

        b2Entities =
            List.map
                (\k ->
                    let
                        ( x, y ) =
                            spiralPos quadB2 k sizeB2
                    in
                    makeEntity ("b2_" ++ String.fromInt k) x y GroupB2 (uppercaseLetter k)
                )
                (List.range 0 (sizeB2 - 1))

        -- Pushout entities: place blob members near blob centers
        blobCenters =
            computeBlobCenters viewMode equivClasses

        pbEntities =
            List.concatMap
                (\cls ->
                    let
                        blobCenter =
                            Dict.get (blobKey cls) blobCenters
                                |> Maybe.withDefault quadP

                        members =
                            List.indexedMap Tuple.pair cls
                    in
                    List.map
                        (\( mi, memberId ) ->
                            let
                                angle =
                                    toFloat mi * 2.3998632

                                r =
                                    12 * sqrt (toFloat (mi + 1) / toFloat (max (List.length cls) 1))

                                x =
                                    blobCenter.x + r * cos angle

                                y =
                                    blobCenter.y + r * sin angle

                                group =
                                    if String.startsWith "pb1_" memberId then
                                        GroupPB1

                                    else
                                        GroupPB2
                            in
                            makeEntity memberId x y group (labelForId memberId)
                        )
                        members
                )
                equivClasses
    in
    aEntities ++ b1Entities ++ b2Entities ++ pbEntities


blobKey : List String -> String
blobKey cls =
    List.sort cls |> String.join ","


computeBlobCenters : ViewMode -> List (List String) -> Dict String { x : Float, y : Float }
computeBlobCenters viewMode classes =
    let
        n =
            List.length classes

        center =
            case viewMode of
                FourQuadrant ->
                    quadP

                ZoomedPushout ->
                    { x = canvasW / 2, y = canvasH / 2 }

        radius =
            case viewMode of
                FourQuadrant ->
                    min 120 (40 * sqrt (toFloat (max n 1)))

                ZoomedPushout ->
                    min 220 (60 * sqrt (toFloat (max n 1)))
    in
    classes
        |> List.indexedMap
            (\i cls ->
                let
                    angle =
                        if n <= 1 then
                            0

                        else
                            toFloat i * 2 * pi / toFloat n

                    r =
                        if n <= 1 then
                            0

                        else
                            radius

                    cx =
                        center.x + r * cos angle

                    cy =
                        center.y + r * sin angle
                in
                ( blobKey cls, { x = cx, y = cy } )
            )
        |> Dict.fromList


buildForces : ViewMode -> List Int -> List Int -> List (List String) -> List ForceEntity -> List (Force.Force String)
buildForces viewMode funcF funcG equivClasses entities =
    let
        allIds =
            List.map .id entities

        blobCenters =
            computeBlobCenters viewMode equivClasses

        -- Intra-blob links
        blobLinks =
            List.concatMap
                (\cls ->
                    case cls of
                        [] ->
                            []

                        first :: rest ->
                            List.map (\other -> ( first, other )) rest
                )
                equivClasses

        blobLinkDistance =
            case viewMode of
                FourQuadrant ->
                    20

                ZoomedPushout ->
                    30

        -- In View 2, link A nodes to their pb targets so they're pulled into blobs
        aToBlobLinks =
            case viewMode of
                FourQuadrant ->
                    []

                ZoomedPushout ->
                    List.indexedMap
                        (\i fi ->
                            { source = "a_" ++ String.fromInt i
                            , target = "pb1_" ++ String.fromInt fi
                            , distance = 10
                            , strength = Just 0.8
                            }
                        )
                        funcF
                        ++ List.indexedMap
                            (\i gi ->
                                { source = "a_" ++ String.fromInt i
                                , target = "pb2_" ++ String.fromInt gi
                                , distance = 10
                                , strength = Just 0.8
                                }
                            )
                            funcG

        allLinkConfigs =
            List.map
                (\( s, t ) ->
                    { source = s, target = t, distance = blobLinkDistance, strength = Just 0.4 }
                )
                blobLinks
                ++ aToBlobLinks

        linkForce =
            if List.isEmpty allLinkConfigs then
                []

            else
                [ Force.customLinks 1 allLinkConfigs ]

        posStrength =
            case viewMode of
                FourQuadrant ->
                    0.04

                ZoomedPushout ->
                    0.03

        -- Per-entity position targets
        posTargets =
            List.filterMap (entityPosTarget viewMode funcF funcG equivClasses blobCenters posStrength) entities

        xTargets =
            List.map (\{ node, strength, target } -> { node = node, strength = strength, target = target.x }) posTargets

        yTargets =
            List.map (\{ node, strength, target } -> { node = node, strength = strength, target = target.y }) posTargets
    in
    [ Force.towardsX xTargets
    , Force.towardsY yTargets
    , Force.collision 20 allIds
    , Force.manyBodyStrength -40 allIds
    ]
        ++ linkForce


entityPosTarget : ViewMode -> List Int -> List Int -> List (List String) -> Dict String { x : Float, y : Float } -> Float -> ForceEntity -> Maybe { node : String, strength : Float, target : { x : Float, y : Float } }
entityPosTarget viewMode funcF funcG equivClasses blobCenters strength ent =
    let
        id =
            ent.id

        simple target =
            Just { node = id, strength = strength, target = target }

        pbPos =
            pbTarget blobCenters equivClasses id
    in
    case viewMode of
        FourQuadrant ->
            case ent.value.group of
                GroupA ->
                    simple quadA

                GroupB1 ->
                    simple quadB1

                GroupB2 ->
                    simple quadB2

                GroupPB1 ->
                    simple pbPos

                GroupPB2 ->
                    simple pbPos

        ZoomedPushout ->
            case ent.value.group of
                GroupA ->
                    Just { node = id, strength = strength * 3, target = aTargetInView2 funcF funcG equivClasses blobCenters id }

                GroupB1 ->
                    Just { node = id, strength = strength * 1.5, target = pbTarget blobCenters equivClasses ("p" ++ id) }

                GroupB2 ->
                    Just { node = id, strength = strength * 1.5, target = pbTarget blobCenters equivClasses ("p" ++ id) }

                GroupPB1 ->
                    simple pbPos

                GroupPB2 ->
                    simple pbPos


pbTarget : Dict String { x : Float, y : Float } -> List (List String) -> String -> { x : Float, y : Float }
pbTarget blobCenters equivClasses id =
    let
        cls =
            List.filter (List.member id) equivClasses |> List.head |> Maybe.withDefault [ id ]
    in
    Dict.get (blobKey cls) blobCenters |> Maybe.withDefault quadP


aTargetInView2 : List Int -> List Int -> List (List String) -> Dict String { x : Float, y : Float } -> String -> { x : Float, y : Float }
aTargetInView2 funcF funcG equivClasses blobCenters aId =
    let
        idx =
            String.dropLeft 2 aId |> String.toInt |> Maybe.withDefault 0

        maybePb1 =
            List.head (List.drop idx funcF)
                |> Maybe.map (\fi -> "pb1_" ++ String.fromInt fi)

        maybePb2 =
            List.head (List.drop idx funcG)
                |> Maybe.map (\gi -> "pb2_" ++ String.fromInt gi)

        maybeTargetId =
            case maybePb1 of
                Just _ ->
                    maybePb1

                Nothing ->
                    maybePb2
    in
    case maybeTargetId of
        Just targetId ->
            pbTarget blobCenters equivClasses targetId

        Nothing ->
            { x = canvasW / 2, y = canvasH / 2 }


rebuildSimulation : ViewMode -> Model -> Model
rebuildSimulation viewMode model =
    let
        equivClasses =
            computeEquivClasses model.sizeB1 model.sizeB2 model.funcF model.funcG

        entities =
            ensureEntities model.sizeA model.sizeB1 model.sizeB2 equivClasses viewMode model.entities

        forces =
            buildForces viewMode model.funcF model.funcG equivClasses entities
    in
    { model
        | equivClasses = equivClasses
        , entities = entities
        , forceState = Force.simulation forces |> Force.iterations 800
        , currentView = viewMode
    }


transitionToView : ViewMode -> Model -> Model
transitionToView viewMode model =
    let
        equivClasses =
            computeEquivClasses model.sizeB1 model.sizeB2 model.funcF model.funcG

        -- Build forces for the new view but keep current entity positions
        newEntities =
            ensureEntities model.sizeA model.sizeB1 model.sizeB2 equivClasses viewMode model.entities

        forces =
            buildForces viewMode model.funcF model.funcG equivClasses newEntities

        -- Run simulation to completion to get final target positions
        targetEntities =
            runSimulationToCompletion
                (Force.simulation forces |> Force.iterations 800)
                newEntities

        startPositions =
            entitiesToPositions newEntities

        finalPositions =
            entitiesToPositions targetEntities

        -- Compute mid-positions: the FQ layout but with B1/B2 at their pb positions
        -- This is the boundary between merge and rearrange phases
        midPositions =
            computeMidPositions viewMode startPositions finalPositions

        ( phase1Targets, phase1Phase ) =
            case viewMode of
                ZoomedPushout ->
                    -- FQ -> ZP: first merge B1/B2 into pb, then rearrange
                    ( midPositions
                    , MergePhase { nextTargets = finalPositions }
                    )

                FourQuadrant ->
                    -- ZP -> FQ: first rearrange to FQ layout (B1/B2 at pb), then split
                    ( midPositions
                    , RearrangePhase { nextTargets = finalPositions }
                    )
    in
    { model
        | equivClasses = equivClasses
        , entities = newEntities
        , forceState = Force.simulation []
        , currentView = viewMode
        , transition =
            Just
                { startPositions = startPositions
                , targetPositions = phase1Targets
                , progress = 0
                , phase = phase1Phase
                }
    }


entitiesToPositions : List ForceEntity -> Dict String { x : Float, y : Float }
entitiesToPositions entities =
    List.foldl (\e d -> Dict.insert e.id { x = e.x, y = e.y } d) Dict.empty entities


{-| Compute intermediate positions where B1/B2 are at their pb counterpart positions.
For FQ->ZP: start from current positions, override B1/B2 to pb current positions.
For ZP->FQ: start from final FQ positions, override B1/B2 to pb final positions.
-}
computeMidPositions : ViewMode -> Dict String { x : Float, y : Float } -> Dict String { x : Float, y : Float } -> Dict String { x : Float, y : Float }
computeMidPositions targetView startPositions finalPositions =
    let
        -- The base positions to start from
        basePositions =
            case targetView of
                ZoomedPushout ->
                    -- Phase 1 is merge: only B1/B2 move, everything else stays
                    startPositions

                FourQuadrant ->
                    -- Phase 1 is rearrange: use final FQ positions but B1/B2 at pb spots
                    finalPositions

        -- Override B1/B2 positions with their corresponding pb positions
        overrideB1B2 id pos =
            if String.startsWith "b1_" id || String.startsWith "b2_" id then
                Dict.get ("p" ++ id) basePositions |> Maybe.withDefault pos

            else
                pos
    in
    Dict.map overrideB1B2 basePositions


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


{-| Ensure all needed entities exist, preserving positions of existing ones.
New entities get default positions.
-}
ensureEntities : Int -> Int -> Int -> List (List String) -> ViewMode -> List ForceEntity -> List ForceEntity
ensureEntities sizeA sizeB1 sizeB2 equivClasses viewMode oldEntities =
    let
        oldDict =
            List.foldl (\e d -> Dict.insert e.id e d) Dict.empty oldEntities

        freshEntities =
            buildEntities sizeA sizeB1 sizeB2 equivClasses viewMode
    in
    List.map
        (\fresh ->
            case Dict.get fresh.id oldDict of
                Just old ->
                    -- Preserve position and velocity, update group info
                    { old | value = fresh.value }

                Nothing ->
                    fresh
        )
        freshEntities



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
                                -- FinalPhase: all done, resume force simulation
                                let
                                    forces =
                                        buildForces model.currentView model.funcF model.funcG model.equivClasses interpolatedEntities
                                in
                                ( { model
                                    | entities = interpolatedEntities
                                    , transition = Nothing
                                    , forceState = Force.simulation forces |> Force.iterations 300
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
                    if Force.isCompleted model.forceState then
                        ( model, Cmd.none )

                    else
                        let
                            ( newState, newEntities ) =
                                Force.tick model.forceState model.entities
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

        SwitchView viewMode ->
            if viewMode == model.currentView then
                ( model, Cmd.none )

            else
                ( transitionToView viewMode model, Cmd.none )

        ArrowDragStart func aIndex pos ->
            ( { model | drag = Just (DraggingArrow { func = func, aIndex = aIndex, mousePos = pos }) }
            , Cmd.none
            )

        NodeDragStart entityId ( cx, cy ) ->
            let
                svgX =
                    cx - model.svgOffset.x

                svgY =
                    cy - model.svgOffset.y

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
            ( { model | drag = Just (DraggingNode { entityId = entityId, offset = offsetXY }) }
            , Cmd.none
            )

        MouseMove ( cx, cy ) ->
            case model.drag of
                Just (DraggingArrow ds) ->
                    ( { model | drag = Just (DraggingArrow { ds | mousePos = ( cx, cy ) }) }, Cmd.none )

                Just (DraggingNode ds) ->
                    let
                        svgX =
                            cx - model.svgOffset.x - Tuple.first ds.offset

                        svgY =
                            cy - model.svgOffset.y - Tuple.second ds.offset

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
                        , forceState = Force.reheat model.forceState
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MouseUp ( mx, my ) ->
            case model.drag of
                Just (DraggingArrow ds) ->
                    let
                        svgX =
                            mx - model.svgOffset.x

                        svgY =
                            my - model.svgOffset.y

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
                Ok element ->
                    ( { model | svgOffset = { x = element.element.x, y = element.element.y } }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



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
            if model.transition /= Nothing || not (Force.isCompleted model.forceState) then
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
    dragSubs



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.style "font-family" "monospace"
        , Attr.style "max-width" "850px"
        , Attr.style "margin" "10px auto"
        ]
        [ Html.h2 [ Attr.style "margin" "0 0 10px" ] [ Html.text "Pushout Construction: B1 <- A -> B2" ]
        , viewControls model
        , viewCanvas model
        , viewLegend model
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        transitioning =
            model.transition /= Nothing
    in
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "gap" "20px"
        , Attr.style "align-items" "center"
        , Attr.style "margin-bottom" "10px"
        , Attr.style "flex-wrap" "wrap"
        ]
        [ viewSlider "|A|" model.sizeA (SetSize SetA) colorA
        , viewSlider "|B1|" model.sizeB1 (SetSize SetB1) colorB1
        , viewSlider "|B2|" model.sizeB2 (SetSize SetB2) colorB2
        , Html.div [ Attr.style "display" "flex" ]
            [ viewToggleButton "Four-Quadrant" FourQuadrant model.currentView transitioning "4px 0 0 4px"
            , viewToggleButton "Zoomed Pushout" ZoomedPushout model.currentView transitioning "0 4px 4px 0"
            ]
        , Html.button
            [ Html.Events.onClick RandomizeFunctions
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
            , Attr.style "border" "1px solid #ccc"
            , Attr.style "border-radius" "4px"
            , Attr.style "background" "#fff"
            , Attr.style "opacity"
                (if transitioning then
                    "0.5"

                 else
                    "1"
                )
            ]
            [ Html.text "Randomize f,g" ]
        ]


viewSlider : String -> Int -> (String -> Msg) -> String -> Html Msg
viewSlider label val toMsg color =
    Html.div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "6px" ]
        [ Html.span [ Attr.style "color" color, Attr.style "font-weight" "bold", Attr.style "min-width" "40px" ]
            [ Html.text (label ++ " = " ++ String.fromInt val) ]
        , Html.input
            [ Attr.type_ "range"
            , Attr.min "0"
            , Attr.max "10"
            , Attr.value (String.fromInt val)
            , Html.Events.onInput toMsg
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


viewCanvas : Model -> Html Msg
viewCanvas model =
    Svg.svg
        [ SA.id "pushout-svg"
        , SA.width (String.fromFloat canvasW)
        , SA.height (String.fromFloat canvasH)
        , SA.viewBox ("0 0 " ++ String.fromFloat canvasW ++ " " ++ String.fromFloat canvasH)
        , Attr.style "background" "#fafafa"
        , Attr.style "border" "1px solid #ddd"
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
                    viewArrow entDict ("a_" ++ String.fromInt i) ("b1_" ++ String.fromInt fi) "arrowF" colorB1 0.6 False (Just ( FuncF, i ))
            )
            model.funcF
            ++ List.indexedMap
                (\i gi ->
                    if isDragging FuncG i then
                        Svg.g [] []

                    else
                        viewArrow entDict ("a_" ++ String.fromInt i) ("b2_" ++ String.fromInt gi) "arrowG" colorB2 0.6 False (Just ( FuncG, i ))
                )
                model.funcG
            ++ List.concatMap
                (\j ->
                    [ viewArrow entDict ("b1_" ++ String.fromInt j) ("pb1_" ++ String.fromInt j) "arrowInject" "#999" 0.3 True Nothing ]
                )
                (List.range 0 (model.sizeB1 - 1))
            ++ List.concatMap
                (\k ->
                    [ viewArrow entDict ("b2_" ++ String.fromInt k) ("pb2_" ++ String.fromInt k) "arrowInject" "#999" 0.3 True Nothing ]
                )
                (List.range 0 (model.sizeB2 - 1))

    else
        -- ZP-style: a->pb1, a->pb2 (draggable)
        List.indexedMap
            (\i fi ->
                if isDragging FuncF i then
                    Svg.g [] []

                else
                    viewArrow entDict ("a_" ++ String.fromInt i) ("pb1_" ++ String.fromInt fi) "arrowF" colorB1 0.5 False (Just ( FuncF, i ))
            )
            model.funcF
            ++ List.indexedMap
                (\i gi ->
                    if isDragging FuncG i then
                        Svg.g [] []

                    else
                        viewArrow entDict ("a_" ++ String.fromInt i) ("pb2_" ++ String.fromInt gi) "arrowG" colorB2 0.5 False (Just ( FuncG, i ))
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
                    "a_" ++ String.fromInt ds.aIndex

                ( mx, my ) =
                    ds.mousePos

                svgX =
                    mx - model.svgOffset.x

                svgY =
                    my - model.svgOffset.y
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
    List.map viewEntity visible


viewEntity : ForceEntity -> Svg Msg
viewEntity ent =
    let
        color =
            groupColor ent.value.group

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



-- LEGEND


viewLegend : Model -> Html Msg
viewLegend model =
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
        , if not (Force.isCompleted model.forceState) then
            Html.span [ Attr.style "color" "#ff8800" ] [ Html.text "| simulating..." ]

          else
            Html.text ""
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
    List.foldl (\e d -> Dict.insert e.id e d) Dict.empty entities


lowercaseLetter : Int -> String
lowercaseLetter i =
    String.fromChar (Char.fromCode (97 + modBy 26 i))


uppercaseLetter : Int -> String
uppercaseLetter i =
    String.fromChar (Char.fromCode (65 + modBy 26 i))


labelForId : String -> String
labelForId id =
    let
        idx =
            String.split "_" id |> List.drop 1 |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault 0
    in
    if String.startsWith "pb1_" id || String.startsWith "b1_" id then
        lowercaseLetter idx

    else if String.startsWith "pb2_" id || String.startsWith "b2_" id then
        uppercaseLetter idx

    else if String.startsWith "a_" id then
        String.fromInt (idx + 1)

    else
        id


ff : Float -> String
ff =
    String.fromFloat
