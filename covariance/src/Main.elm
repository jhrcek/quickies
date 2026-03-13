module Main exposing (main)

import Browser
import Browser.Events as Events
import Html exposing (Html, button, div, input, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import KaTeX
import List
import Maybe exposing (Maybe(..))
import Random
import Set exposing (Set)
import Svg
import Svg.Attributes as SA
import Svg.Events as SE


type Section
    = MeansSection
    | VarianceSection
    | StdDevSection
    | CovarianceSection
    | PearsonSection


sectionToInt : Section -> Int
sectionToInt section =
    case section of
        MeansSection ->
            0

        VarianceSection ->
            1

        StdDevSection ->
            2

        CovarianceSection ->
            3

        PearsonSection ->
            4


isExpanded : Section -> Model -> Bool
isExpanded section model =
    Set.member (sectionToInt section) model.expandedSections


type alias Model =
    { numPoints : Int
    , points : List ( Float, Float )
    , hoveredIndex : Maybe Int
    , dragState : Maybe DragState
    , expandedSections : Set Int
    }


type alias DragState =
    { index : Int
    , offsetX : Float
    , offsetY : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { numPoints = initNumPoints
      , points = []
      , hoveredIndex = Nothing
      , dragState = Nothing
      , expandedSections =
            Set.fromList
                [ sectionToInt MeansSection
                , sectionToInt VarianceSection
                , sectionToInt StdDevSection
                , sectionToInt CovarianceSection
                , sectionToInt PearsonSection
                ]
      }
    , generatePoints initNumPoints
    )


type Msg
    = ChangeNumPoints String
    | Generate
    | GotRandomPoints (List ( Float, Float ))
    | Hover Int
    | Unhover
    | SummationHover Int
    | SummationUnhover
    | StartDrag Int Float Float
    | Move Float Float
    | StopDrag
    | ToggleSection Section


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNumPoints str ->
            ( { model
                | numPoints =
                    String.toInt str
                        |> Maybe.map (clamp minNumPoints maxNumPoints)
                        |> Maybe.withDefault initNumPoints
              }
            , Cmd.none
            )

        Generate ->
            ( model, generatePoints model.numPoints )

        GotRandomPoints newPoints ->
            ( { model | points = newPoints, hoveredIndex = Nothing }, Cmd.none )

        Hover idx ->
            ( { model | hoveredIndex = Just idx }, Cmd.none )

        Unhover ->
            case model.dragState of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | hoveredIndex = Nothing }, Cmd.none )

        SummationHover idx ->
            ( { model | hoveredIndex = Just idx }, Cmd.none )

        SummationUnhover ->
            case model.dragState of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | hoveredIndex = Nothing }, Cmd.none )

        StartDrag index mouseX mouseY ->
            let
                ( screenX, screenY ) =
                    pointToScreen model index

                offsetX =
                    screenX - mouseX

                offsetY =
                    screenY - mouseY
            in
            ( { model | dragState = Just { index = index, offsetX = offsetX, offsetY = offsetY } }
            , Cmd.none
            )

        Move mouseX mouseY ->
            case model.dragState of
                Just d ->
                    let
                        newScreenX =
                            mouseX + d.offsetX

                        newScreenY =
                            mouseY + d.offsetY

                        ( newDataX, newDataY ) =
                            screenToData newScreenX newScreenY

                        clampedX =
                            clamp 0 1 newDataX

                        clampedY =
                            clamp 0 1 newDataY

                        updatedPoints =
                            List.indexedMap
                                (\i ( oldX, oldY ) ->
                                    if i == d.index then
                                        ( clampedX, clampedY )

                                    else
                                        ( oldX, oldY )
                                )
                                model.points
                    in
                    ( { model | points = updatedPoints }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        StopDrag ->
            ( { model | dragState = Nothing }, Cmd.none )

        ToggleSection section ->
            let
                key =
                    sectionToInt section

                newSections =
                    if Set.member key model.expandedSections then
                        Set.remove key model.expandedSections

                    else
                        Set.insert key model.expandedSections
            in
            ( { model | expandedSections = newSections }, Cmd.none )


generatePoints : Int -> Cmd Msg
generatePoints numPoints =
    Random.generate GotRandomPoints (Random.list numPoints (Random.pair (Random.float 0 1) (Random.float 0 1)))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragState of
        Just _ ->
            Sub.batch
                [ Events.onMouseMove (D.map2 Move (D.field "clientX" D.float) (D.field "clientY" D.float))
                , Events.onMouseUp (D.succeed StopDrag)
                ]

        Nothing ->
            Sub.none


view : Model -> Html Msg
view model =
    div
        [ HA.style "width" "100vw"
        , HA.style "height" "100vh"
        , HA.style "box-sizing" "border-box"
        , HA.style "margin" "0"
        , HA.style "padding" "0"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.h1 [] [ Html.text "Covariance" ]
        , controlsView model
        , div
            [ HA.style "display" "flex"
            , HA.style "flex" "1"
            , HA.style "flex-direction" "row"
            , HA.style "padding" "1rem"
            ]
            [ div
                [ HA.style "flex" "0 0 auto" -- chart gets natural width from the SVG
                ]
                [ viewGraph model
                , div
                    [ HA.style "color" "#aaa"
                    , HA.style "font-size" "13px"
                    , HA.style "margin-top" "4px"
                    ]
                    [ text "Drag and drop points and see how it affects the statistics." ]
                ]
            , div
                [ HA.style "flex" "1"
                , HA.style "margin-left" "1rem"
                , HA.style "overflow" "auto"
                ]
                [ viewMeans model
                , viewVariance model
                , viewStdDev model
                , viewCovariance model
                , viewPearson model
                ]
            ]
        ]


controlsView : Model -> Html Msg
controlsView model =
    div [ HA.style "padding" "1rem" ]
        [ text "Number of points "
        , input
            [ HA.type_ "number"
            , HA.value (String.fromInt model.numPoints)
            , HA.min (String.fromInt minNumPoints)
            , HA.max (String.fromInt maxNumPoints)
            , HE.onInput ChangeNumPoints
            ]
            []
        , button [ HE.onClick Generate ] [ text "Generate Random Points" ]
        ]


viewGraph : Model -> Html Msg
viewGraph model =
    let
        ( meanX, meanY, n ) =
            meanXY model.points

        meanXSvg =
            scaleX meanX

        meanYSvg =
            scaleY meanY

        ( sigmaX, sigmaY ) =
            stdDevXY model.points ( meanX, meanY, n )

        rxSvg =
            sigmaX * (svgWidth - 2 * svgMargin)

        rySvg =
            sigmaY * (svgHeight - 2 * svgMargin)

        highlightedIndex =
            case model.dragState of
                Just d ->
                    Just d.index

                Nothing ->
                    model.hoveredIndex

        highlightRect =
            case highlightedIndex of
                Just i ->
                    case List.head (List.drop i model.points) of
                        Just ( px, py ) ->
                            let
                                pxSvg =
                                    scaleX px

                                pySvg =
                                    scaleY py

                                sign =
                                    (px - meanX) * (py - meanY)

                                fillColor =
                                    if sign >= 0 then
                                        "blue"

                                    else
                                        "red"

                                rectX =
                                    min pxSvg meanXSvg

                                rectY =
                                    min pySvg meanYSvg

                                rectWidth =
                                    abs (pxSvg - meanXSvg)

                                rectHeight =
                                    abs (pySvg - meanYSvg)
                            in
                            [ Svg.rect
                                [ SA.x (String.fromFloat rectX)
                                , SA.y (String.fromFloat rectY)
                                , SA.width (String.fromFloat rectWidth)
                                , SA.height (String.fromFloat rectHeight)
                                , SA.fill fillColor
                                , SA.fillOpacity "0.3"
                                ]
                                []
                            ]

                        Nothing ->
                            []

                Nothing ->
                    []
    in
    Svg.svg
        [ SA.width (String.fromFloat svgWidth)
        , SA.height (String.fromFloat svgHeight)
        , SA.viewBox ("0 0 " ++ String.fromFloat svgWidth ++ " " ++ String.fromFloat svgHeight)
        , SA.style "background-color:white;user-select:none"
        ]
        ([ Svg.line
            [ SA.x1 (String.fromFloat svgMargin)
            , SA.y1 (String.fromFloat (svgHeight - svgMargin))
            , SA.x2 (String.fromFloat (svgWidth - svgMargin))
            , SA.y2 (String.fromFloat (svgHeight - svgMargin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromFloat svgMargin)
            , SA.y1 (String.fromFloat svgMargin)
            , SA.x2 (String.fromFloat svgMargin)
            , SA.y2 (String.fromFloat (svgHeight - svgMargin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromFloat meanXSvg)
            , SA.y1 (String.fromFloat svgMargin)
            , SA.x2 (String.fromFloat meanXSvg)
            , SA.y2 (String.fromFloat (svgHeight - svgMargin))
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromFloat svgMargin)
            , SA.y1 (String.fromFloat meanYSvg)
            , SA.x2 (String.fromFloat (svgWidth - svgMargin))
            , SA.y2 (String.fromFloat meanYSvg)
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []

         -- Mean labels
         , Svg.text_
            [ SA.x (String.fromFloat (meanXSvg + 4))
            , SA.y (String.fromFloat (svgMargin - 6))
            , SA.fill "grey"
            , SA.fontSize "13px"
            ]
            [ Svg.text "x̄" ]
         , Svg.text_
            [ SA.x (String.fromFloat (svgWidth - svgMargin + 6))
            , SA.y (String.fromFloat (meanYSvg - 4))
            , SA.fill "grey"
            , SA.fontSize "13px"
            ]
            [ Svg.text "ȳ" ]
         ]
            ++ (if isExpanded StdDevSection model then
                    [ Svg.ellipse
                        [ SA.cx (String.fromFloat meanXSvg)
                        , SA.cy (String.fromFloat meanYSvg)
                        , SA.rx (String.fromFloat rxSvg)
                        , SA.ry (String.fromFloat rySvg)
                        , SA.fill "none"
                        , SA.stroke "orange"
                        , SA.strokeWidth "1.5"
                        , SA.strokeDasharray "6,3"
                        , SA.opacity "0.7"
                        ]
                        []

                    -- sigma_x labels at ellipse-horizontal intersections
                    , Svg.text_
                        [ SA.x (String.fromFloat (meanXSvg - rxSvg - 8))
                        , SA.y (String.fromFloat (meanYSvg + 18))
                        , SA.fill "orange"
                        , SA.fontSize "14px"
                        , SA.textAnchor "end"
                        ]
                        [ Svg.text "-σx" ]
                    , Svg.text_
                        [ SA.x (String.fromFloat (meanXSvg + rxSvg + 8))
                        , SA.y (String.fromFloat (meanYSvg + 18))
                        , SA.fill "orange"
                        , SA.fontSize "14px"
                        , SA.textAnchor "start"
                        ]
                        [ Svg.text "+σx" ]

                    -- sigma_y labels at ellipse-vertical intersections
                    , Svg.text_
                        [ SA.x (String.fromFloat (meanXSvg + 6))
                        , SA.y (String.fromFloat (meanYSvg + rySvg + 16))
                        , SA.fill "orange"
                        , SA.fontSize "14px"
                        ]
                        [ Svg.text "-σy" ]
                    , Svg.text_
                        [ SA.x (String.fromFloat (meanXSvg + 6))
                        , SA.y (String.fromFloat (meanYSvg - rySvg - 6))
                        , SA.fill "orange"
                        , SA.fontSize "14px"
                        ]
                        [ Svg.text "+σy" ]
                    ]

                else
                    []
               )
            ++ highlightRect
            ++ List.indexedMap viewPoint model.points
        )


viewPoint :
    Int
    -> ( Float, Float )
    -> Svg.Svg Msg
viewPoint idx ( x, y ) =
    let
        cx_ =
            scaleX x

        cy_ =
            scaleY y

        titleText =
            "(" ++ round3DP x ++ ", " ++ round3DP y ++ ")"
    in
    Svg.circle
        [ SA.cx (String.fromFloat cx_)
        , SA.cy (String.fromFloat cy_)
        , SA.r "5"
        , SA.fill "black"
        , onMouseDownPos (StartDrag idx)
        , SE.onMouseOver (Hover idx)
        , SE.onMouseOut Unhover
        ]
        [ Svg.title []
            [ Svg.text titleText ]
        ]


meanXY : List ( Float, Float ) -> ( Float, Float, Float )
meanXY points =
    if List.isEmpty points then
        ( 0, 0, 0 )

    else
        let
            n =
                toFloat (List.length points)

            ( sumX, sumY ) =
                List.foldl (\( x, y ) ( accX, accY ) -> ( x + accX, y + accY )) ( 0, 0 ) points
        in
        ( sumX / n, sumY / n, n )


varianceXY : List ( Float, Float ) -> ( Float, Float, Float ) -> ( Float, Float )
varianceXY points ( meanX, meanY, n ) =
    if n == 0 then
        ( 0, 0 )

    else
        let
            ( sumSqX, sumSqY ) =
                List.foldl
                    (\( x, y ) ( accX, accY ) -> ( accX + (x - meanX) ^ 2, accY + (y - meanY) ^ 2 ))
                    ( 0, 0 )
                    points
        in
        ( sumSqX / n, sumSqY / n )


stdDevXY : List ( Float, Float ) -> ( Float, Float, Float ) -> ( Float, Float )
stdDevXY points means =
    varianceXY points means
        |> Tuple.mapBoth sqrt sqrt


covarianceXY : List ( Float, Float ) -> ( Float, Float, Float ) -> Float
covarianceXY points ( meanX, meanY, n ) =
    if n == 0 then
        0

    else
        List.foldl (\( x, y ) acc -> acc + (x - meanX) * (y - meanY)) 0 points / n


viewAccordion : Bool -> Msg -> String -> List (Html Msg) -> Html Msg
viewAccordion isOpen toggleMsg title content =
    Html.details
        ([ HA.style "margin-top" "16px"
         , HA.style "border" "1px solid #ddd"
         , HA.style "border-radius" "4px"
         , HA.style "padding" "12px"
         , HE.on "toggle"
            (D.at [ "target", "open" ] D.bool
                |> D.andThen
                    (\open ->
                        if open /= isOpen then
                            D.succeed toggleMsg

                        else
                            D.fail "no change"
                    )
            )
         ]
            ++ (if isOpen then
                    [ HA.attribute "open" "" ]

                else
                    []
               )
        )
        (Html.summary
            [ HA.style "cursor" "pointer"
            , HA.style "font-weight" "bold"
            , HA.style "font-size" "16px"
            ]
            [ Html.text title ]
            :: content
        )


viewMeans : Model -> Html Msg
viewMeans model =
    let
        ( meanX, meanY, _ ) =
            meanXY model.points
    in
    viewAccordion (isExpanded MeansSection model)
        (ToggleSection MeansSection)
        "Means x̄, ȳ"
        [ div [] [ KaTeX.inline ("\\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i = " ++ round3DP meanX) ]
        , div [] [ KaTeX.inline ("\\bar{y} = \\frac{1}{n} \\sum_{i=1}^{n} y_i = " ++ round3DP meanY) ]
        ]


viewVariance : Model -> Html Msg
viewVariance model =
    let
        ( meanX, meanY, n ) =
            meanXY model.points

        ( varX, varY ) =
            varianceXY model.points ( meanX, meanY, n )
    in
    viewAccordion (isExpanded VarianceSection model)
        (ToggleSection VarianceSection)
        "Variance"
        [ div [] [ KaTeX.inline ("\\sigma_x^2 = \\frac{1}{n} \\sum_{i=1}^{n} (x_i - \\bar{x})^2 = " ++ round3DP varX) ]
        , div [] [ KaTeX.inline ("\\sigma_y^2 = \\frac{1}{n} \\sum_{i=1}^{n} (y_i - \\bar{y})^2 = " ++ round3DP varY) ]
        ]


viewStdDev : Model -> Html Msg
viewStdDev model =
    let
        ( meanX, meanY, n ) =
            meanXY model.points

        ( sigmaX, sigmaY ) =
            stdDevXY model.points ( meanX, meanY, n )
    in
    viewAccordion (isExpanded StdDevSection model)
        (ToggleSection StdDevSection)
        "Standard Deviation"
        [ div [] [ KaTeX.inline ("\\sigma_x = \\sqrt{\\sigma_x^2} = " ++ round3DP sigmaX) ]
        , div [] [ KaTeX.inline ("\\sigma_y = \\sqrt{\\sigma_y^2} = " ++ round3DP sigmaY) ]
        ]


viewCovariance : Model -> Html Msg
viewCovariance model =
    let
        ( meanX, meanY, n ) =
            meanXY model.points

        terms =
            List.map (\( x, y ) -> (x - meanX) * (y - meanY)) model.points

        sumTerms =
            List.foldl (+) 0 terms

        totalCov =
            if n == 0 then
                0

            else
                sumTerms / n
    in
    viewAccordion (isExpanded CovarianceSection model)
        (ToggleSection CovarianceSection)
        "Covariance"
        [ div [] [ KaTeX.inline "\\text{Cov}(X,Y) = \\frac{1}{n} \\sum_{i=1}^{n} (x_i - \\bar{x})(y_i - \\bar{y})" ]
        , div
            [ HA.style "display" "grid"
            , HA.style "grid-template-columns" "auto auto auto"
            , HA.style "column-gap" "12px"
            , HA.style "row-gap" "2px"
            , HA.style "align-items" "center"
            , HA.style "margin-top" "8px"
            , HA.style "font-size" "14px"
            ]
            (List.concat (List.indexedMap (viewCovTerm meanX meanY model.hoveredIndex) model.points))
        , div [ HA.style "margin-top" "8px" ]
            [ KaTeX.inline ("= \\frac{" ++ round3DP sumTerms ++ "}{" ++ String.fromFloat n ++ "} = " ++ round3DP totalCov) ]
        ]


viewCovTerm : Float -> Float -> Maybe Int -> Int -> ( Float, Float ) -> List (Html Msg)
viewCovTerm meanX meanY hoveredIndex i ( x, y ) =
    let
        dx =
            x - meanX

        dy =
            y - meanY

        product =
            dx * dy

        color =
            if product >= 0 then
                "blue"

            else
                "red"

        bg =
            case hoveredIndex of
                Just hI ->
                    if hI == i then
                        if product >= 0 then
                            "lightblue"

                        else
                            "pink"

                    else
                        "transparent"

                Nothing ->
                    "transparent"

        rowAttrs =
            [ HA.style "color" color
            , HA.style "background-color" bg
            , HA.style "padding" "2px 4px"
            , HE.onMouseOver (SummationHover i)
            , HE.onMouseOut SummationUnhover
            ]

        plus =
            if i > 0 then
                "+ \\;"

            else
                "\\phantom{+} \\;"
    in
    [ div rowAttrs [ KaTeX.inline (plus ++ "(" ++ round3DP x ++ " - " ++ round3DP meanX ++ ")(" ++ round3DP y ++ " - " ++ round3DP meanY ++ ")") ]
    , div rowAttrs [ KaTeX.inline ("= (" ++ round3DP dx ++ ")(" ++ round3DP dy ++ ")") ]
    , div rowAttrs [ KaTeX.inline ("= " ++ round3DP product) ]
    ]


viewPearson : Model -> Html Msg
viewPearson model =
    let
        ( meanX, meanY, n ) =
            meanXY model.points

        totalCov =
            covarianceXY model.points ( meanX, meanY, n )

        ( sigmaX, sigmaY ) =
            stdDevXY model.points ( meanX, meanY, n )

        pearson =
            if sigmaX * sigmaY == 0 then
                0

            else
                totalCov / (sigmaX * sigmaY)
    in
    viewAccordion (isExpanded PearsonSection model)
        (ToggleSection PearsonSection)
        "Pearson Correlation Coefficient"
        [ div [] [ KaTeX.inline ("\\rho_{xy} = \\frac{\\text{Cov}(X,Y)}{\\sigma_x \\sigma_y} = " ++ round3DP pearson) ]
        ]


onMouseDownPos : (Float -> Float -> msg) -> Svg.Attribute msg
onMouseDownPos toMsg =
    let
        decoder =
            D.map2 toMsg
                (D.field "clientX" D.float)
                (D.field "clientY" D.float)
    in
    HE.on "mousedown" decoder


pointToScreen : Model -> Int -> ( Float, Float )
pointToScreen model i =
    let
        ( x, y ) =
            List.head (List.drop i model.points)
                |> Maybe.withDefault ( 0, 0 )
    in
    ( scaleX x, scaleY y )


screenToData : Float -> Float -> ( Float, Float )
screenToData screenX screenY =
    let
        dataX =
            (screenX - svgMargin) / (svgWidth - 2 * svgMargin)

        dataY =
            (svgHeight - svgMargin - screenY) / (svgHeight - 2 * svgMargin)
    in
    ( dataX, dataY )


scaleX : Float -> Float
scaleX x =
    svgMargin + x * (svgWidth - 2 * svgMargin)


scaleY : Float -> Float
scaleY y =
    svgHeight - svgMargin - y * (svgHeight - 2 * svgMargin)


svgWidth : Float
svgWidth =
    800


svgHeight : Float
svgHeight =
    600


svgMargin : Float
svgMargin =
    40


initNumPoints : Int
initNumPoints =
    10


minNumPoints : Int
minNumPoints =
    1


maxNumPoints : Int
maxNumPoints =
    100


round3DP : Float -> String
round3DP val =
    let
        rounded =
            round (val * 1000)
    in
    String.fromFloat (toFloat rounded / 1000)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
