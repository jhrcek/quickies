module Main exposing (main)

{-| Interactive explainer of the four types of solar eclipse
(totale, ringförmige, partielle and hybride Sonnenfinsternis).

The scene is purely schematic (not to scale). The user controls the
Earth-Moon distance and the north-south offset of the Moon's path,
which determines where umbra / penumbra / antumbra fall and hence the
eclipse type. A third slider moves an observer along the eclipse path,
which is the key to understanding the hybrid eclipse: the middle of the
path is up to one Earth radius closer to the Moon than its start/end.

-}

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg as S exposing (Svg)
import Svg.Attributes as SA



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { moonDistance : Float -- schematic Earth-Moon distance in scene units
    , moonOffset : Float -- north-south offset of the Moon from the Sun-Earth line
    , observerAngle : Float -- observer position along the eclipse path, in degrees from the sub-solar point
    }


init : Model
init =
    { moonDistance = 154, moonOffset = 0, observerAngle = 0 }



-- UPDATE


type Msg
    = DistanceChanged String
    | OffsetChanged String
    | ObserverChanged String
    | PresetClicked Float Float Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        DistanceChanged s ->
            { model | moonDistance = parseWithDefault model.moonDistance s }

        OffsetChanged s ->
            { model | moonOffset = parseWithDefault model.moonOffset s }

        ObserverChanged s ->
            { model | observerAngle = parseWithDefault model.observerAngle s }

        PresetClicked distance offset observer ->
            { model | moonDistance = distance, moonOffset = offset, observerAngle = observer }


parseWithDefault : Float -> String -> Float
parseWithDefault default s =
    Maybe.withDefault default (String.toFloat s)



-- GEOMETRY (schematic scene units)


sceneWidth : Float
sceneWidth =
    940


sceneHeight : Float
sceneHeight =
    420


sunX : Float
sunX =
    80


centerY : Float
centerY =
    210


sunR : Float
sunR =
    70


moonR : Float
moonR =
    13


earthX : Float
earthX =
    800


earthR : Float
earthR =
    50


type alias Point =
    { x : Float, y : Float }


sunCenter : Point
sunCenter =
    { x = sunX, y = centerY }


vAdd : Point -> Point -> Point
vAdd a b =
    { x = a.x + b.x, y = a.y + b.y }


vSub : Point -> Point -> Point
vSub a b =
    { x = a.x - b.x, y = a.y - b.y }


vScale : Float -> Point -> Point
vScale k a =
    { x = k * a.x, y = k * a.y }


vLen : Point -> Float
vLen a =
    sqrt (a.x * a.x + a.y * a.y)


type EclipseType
    = Total
    | Annular
    | Hybrid
    | Partial
    | NoEclipse


isCentral : EclipseType -> Bool
isCentral t =
    case t of
        Total ->
            True

        Annular ->
            True

        Hybrid ->
            True

        _ ->
            False


type alias Geo =
    { moon : Point
    , sunMoonDist : Float
    , axisDir : Point -- unit vector Sun -> Moon
    , axisPerp : Point -- unit vector perpendicular to the shadow axis
    , apex : Point -- tip of the umbra cone
    , apexLen : Float -- distance from Sun center to umbra apex, along the axis
    , penumbraApexLen : Float -- distance from Sun center to the penumbra cone apex
    , eclipse : EclipseType
    }


deriveGeo : Model -> Geo
deriveGeo model =
    let
        moon =
            { x = earthX - model.moonDistance, y = centerY + model.moonOffset }

        d =
            vSub moon sunCenter

        l =
            vLen d

        u =
            vScale (1 / l) d

        n =
            { x = -u.y, y = u.x }

        -- external tangents of Sun and Moon meet here (tip of the umbra)
        apexLen =
            l * sunR / (sunR - moonR)

        apex =
            vAdd sunCenter (vScale apexLen u)

        -- internal tangents cross between Sun and Moon (apex of the penumbra cone)
        penumbraApexLen =
            l * sunR / (sunR + moonR)

        axisYAtEarth =
            centerY + (moon.y - centerY) * (earthX - sunX) / (moon.x - sunX)

        missDist =
            abs (axisYAtEarth - centerY)

        eclipse =
            if missDist < earthR then
                let
                    xNearSurface =
                        earthX - sqrt (earthR * earthR - missDist * missDist)
                in
                if apex.x < xNearSurface then
                    Annular

                else if apex.x >= earthX then
                    Total

                else
                    -- the umbra tip ends inside the Earth: the middle of the
                    -- eclipse path reaches into the umbra (total), the ends of
                    -- the path lie beyond the tip in the antumbra (annular)
                    Hybrid

            else
                let
                    tEarth =
                        (earthX - sunX) / u.x

                    penumbraHalfWidth =
                        (tEarth - penumbraApexLen) * (sunR + moonR) / l
                in
                if missDist < earthR + penumbraHalfWidth then
                    Partial

                else
                    NoEclipse
    in
    { moon = moon
    , sunMoonDist = l
    , axisDir = u
    , axisPerp = n
    , apex = apex
    , apexLen = apexLen
    , penumbraApexLen = penumbraApexLen
    , eclipse = eclipse
    }


{-| Point on the sunlit side of the Earth, parameterized by the angle (in
degrees) from the point closest to the Sun.
-}
observerPoint : Float -> Point
observerPoint angleDeg =
    { x = earthX - earthR * cos (degrees angleDeg)
    , y = centerY + earthR * sin (degrees angleDeg)
    }


{-| Apparent size of the Moon relative to the Sun for an observer on the
eclipse path (assuming the Moon is centrally aligned at the observer's
moment of maximum eclipse). >= 1 means the Moon covers the Sun completely.
-}
centralSizeRatio : Geo -> Float -> Float
centralSizeRatio g angleDeg =
    let
        o =
            observerPoint angleDeg

        dMoon =
            vLen (vSub g.moon o)

        dSun =
            vLen (vSub sunCenter o)
    in
    (moonR / dMoon) / (sunR / dSun)


type Seen
    = SeenTotal Float -- apparent Moon/Sun size ratio
    | SeenAnnular Float
    | SeenPartial Float Float -- size ratio, apparent separation in Sun radii
    | SeenNothing


observerSees : Geo -> Model -> Seen
observerSees g model =
    if isCentral g.eclipse then
        let
            ratio =
                centralSizeRatio g model.observerAngle
        in
        if ratio >= 1 then
            SeenTotal ratio

        else
            SeenAnnular ratio

    else
        let
            o =
                observerPoint model.observerAngle

            dMoon =
                vLen (vSub g.moon o)

            dSun =
                vLen (vSub sunCenter o)

            moonApp =
                moonR / dMoon

            sunApp =
                sunR / dSun

            -- apparent vertical separation of Moon and Sun centers (small-angle)
            separation =
                (g.moon.y - o.y) / dMoon - (sunCenter.y - o.y) / dSun
        in
        if abs separation < moonApp + sunApp then
            SeenPartial (moonApp / sunApp) (separation / sunApp)

        else
            SeenNothing



-- VIEW


view : Model -> Html Msg
view model =
    let
        g =
            deriveGeo model

        seen =
            observerSees g model
    in
    H.div
        [ HA.style "max-width" "1000px"
        , HA.style "margin" "0 auto"
        , HA.style "padding" "16px"
        , HA.style "font-family" "system-ui, sans-serif"
        , HA.style "color" "#1f2937"
        ]
        [ H.h1 [ HA.style "font-size" "26px", HA.style "margin-bottom" "4px" ]
            [ H.text "Arten der Sonnenfinsternis" ]
        , H.p [ HA.style "font-size" "15px", HA.style "line-height" "1.55", HA.style "color" "#475569" ]
            [ H.text "Welcher Finsternistyp entsteht, hängt davon ab, wohin Kernschatten und Halbschatten des Mondes fallen. Verändere den Abstand des Mondes und den Versatz seiner Bahn – oder wähle eine Voreinstellung. Die Darstellung ist schematisch und nicht maßstabsgetreu." ]
        , viewPresets g.eclipse
        , viewControls model
        , viewBanner g.eclipse
        , viewScene model g
        , viewLegend g.eclipse
        , H.div
            [ HA.style "display" "flex"
            , HA.style "flex-wrap" "wrap"
            , HA.style "gap" "16px"
            , HA.style "align-items" "stretch"
            , HA.style "margin-top" "16px"
            ]
            [ viewObserverPanel seen
            , viewExplanation g.eclipse
            ]
        , H.p [ HA.style "font-size" "12px", HA.style "color" "#94a3b8", HA.style "margin-top" "20px" ]
            [ H.text "Begriffe nach dem Wissenskarten-Artikel der "
            , H.a
                [ HA.href "https://www.medienwerkstatt-online.de/lws_wissen/vorlagen/showcard.php?id=4451"
                , HA.style "color" "#64748b"
                ]
                [ H.text "Medienwerkstatt" ]
            , H.text "."
            ]
        ]



-- PRESETS


type alias Preset =
    { label : String
    , eclipseType : EclipseType
    , distance : Float
    , offset : Float
    , observer : Float
    }


presets : List Preset
presets =
    [ Preset "Partiell" Partial 200 55 70
    , Preset "Total" Total 120 0 0
    , Preset "Ringförmig" Annular 300 0 0
    , Preset "Hybrid" Hybrid 154 0 0
    ]


viewPresets : EclipseType -> Html Msg
viewPresets current =
    H.div
        [ HA.style "display" "flex"
        , HA.style "gap" "8px"
        , HA.style "flex-wrap" "wrap"
        , HA.style "align-items" "center"
        , HA.style "margin" "10px 0"
        ]
        (H.span [ HA.style "font-size" "14px", HA.style "font-weight" "600", HA.style "color" "#334155" ]
            [ H.text "Voreinstellungen:" ]
            :: List.map (presetButton current) presets
        )


presetButton : EclipseType -> Preset -> Html Msg
presetButton current preset =
    let
        active =
            current == preset.eclipseType

        ( bg, fg, border ) =
            if active then
                ( typeColor preset.eclipseType, "#ffffff", typeColor preset.eclipseType )

            else
                ( "#ffffff", "#334155", "#cbd5e1" )
    in
    H.button
        [ HE.onClick (PresetClicked preset.distance preset.offset preset.observer)
        , HA.style "padding" "6px 14px"
        , HA.style "border-radius" "999px"
        , HA.style "border" ("1px solid " ++ border)
        , HA.style "background" bg
        , HA.style "color" fg
        , HA.style "font-size" "14px"
        , HA.style "font-weight" "600"
        , HA.style "cursor" "pointer"
        ]
        [ H.text preset.label ]



-- CONTROLS


viewControls : Model -> Html Msg
viewControls model =
    H.div
        [ HA.style "display" "flex"
        , HA.style "flex-wrap" "wrap"
        , HA.style "gap" "18px"
        , HA.style "margin" "8px 0 14px"
        ]
        [ sliderBlock "Abstand Erde–Mond" "nah" "fern" 100 330 model.moonDistance DistanceChanged
        , sliderBlock "Bahnversatz des Mondes" "nördlich" "südlich" -80 80 model.moonOffset OffsetChanged
        , sliderBlock "Beobachter auf dem Finsternispfad" "Anfang" "Ende" -85 85 model.observerAngle ObserverChanged
        ]


sliderBlock : String -> String -> String -> Float -> Float -> Float -> (String -> Msg) -> Html Msg
sliderBlock label leftHint rightHint minV maxV val toMsg =
    H.div [ HA.style "flex" "1", HA.style "min-width" "240px" ]
        [ H.div
            [ HA.style "font-size" "14px"
            , HA.style "font-weight" "600"
            , HA.style "color" "#334155"
            , HA.style "margin-bottom" "2px"
            ]
            [ H.text label ]
        , H.input
            [ HA.type_ "range"
            , HA.min (fs minV)
            , HA.max (fs maxV)
            , HA.step "1"
            , HA.value (fs val)
            , HE.onInput toMsg
            , HA.style "width" "100%"
            ]
            []
        , H.div
            [ HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "font-size" "12px"
            , HA.style "color" "#64748b"
            ]
            [ H.text leftHint, H.text rightHint ]
        ]



-- CURRENT TYPE BANNER


viewBanner : EclipseType -> Html Msg
viewBanner t =
    H.div [ HA.style "margin" "6px 0 10px", HA.style "font-size" "17px" ]
        [ H.text "Aktueller Typ: "
        , H.span
            [ HA.style "background" (typeColor t)
            , HA.style "color" "#ffffff"
            , HA.style "padding" "3px 14px"
            , HA.style "border-radius" "999px"
            , HA.style "font-weight" "700"
            ]
            [ H.text (typeName t) ]
        ]



-- MAIN SCENE


viewScene : Model -> Geo -> Html Msg
viewScene model g =
    S.svg
        [ SA.viewBox ("0 0 " ++ fs sceneWidth ++ " " ++ fs sceneHeight)
        , HA.style "width" "100%"
        , HA.style "height" "auto"
        , HA.style "background" "#f4f8fd"
        , HA.style "border" "1px solid #dbe4ee"
        , HA.style "border-radius" "12px"
        ]
        ([ sceneDefs
         , S.circle
            [ SA.cx (fs earthX)
            , SA.cy (fs centerY)
            , SA.r (fs earthR)
            , SA.fill "url(#earthGrad)"
            , SA.stroke "#28527a"
            , SA.strokeWidth "1.5"
            ]
            []
         ]
            ++ shadowPolygons g
            ++ [ axisLine g
               , S.circle
                    [ SA.cx (fs sunX)
                    , SA.cy (fs centerY)
                    , SA.r (fs (sunR + 12))
                    , SA.fill "#ffb703"
                    , SA.opacity "0.18"
                    ]
                    []
               , S.circle
                    [ SA.cx (fs sunX)
                    , SA.cy (fs centerY)
                    , SA.r (fs sunR)
                    , SA.fill "url(#sunGrad)"
                    ]
                    []
               , S.circle
                    [ SA.cx (fs g.moon.x)
                    , SA.cy (fs g.moon.y)
                    , SA.r (fs moonR)
                    , SA.fill "#8d99ae"
                    , SA.stroke "#4a5568"
                    , SA.strokeWidth "1"
                    ]
                    []
               ]
            ++ (if isCentral g.eclipse then
                    viewPathArcs g

                else
                    []
               )
            ++ apexMarker g
            ++ observerMarker model
            ++ sceneLabels g
        )


sceneDefs : Svg msg
sceneDefs =
    S.defs []
        [ S.radialGradient [ SA.id "sunGrad" ]
            [ S.stop [ SA.offset "0%", SA.stopColor "#fff9d6" ] []
            , S.stop [ SA.offset "60%", SA.stopColor "#ffd23f" ] []
            , S.stop [ SA.offset "100%", SA.stopColor "#fb8b24" ] []
            ]
        , S.radialGradient [ SA.id "earthGrad" ]
            [ S.stop [ SA.offset "0%", SA.stopColor "#7cc0ea" ] []
            , S.stop [ SA.offset "100%", SA.stopColor "#1d4e7e" ] []
            ]
        ]


shadowPolygons : Geo -> List (Svg msg)
shadowPolygons g =
    let
        tEnd =
            (sceneWidth + 10 - sunX) / g.axisDir.x

        pointAt t w =
            vAdd sunCenter (vAdd (vScale t g.axisDir) (vScale w g.axisPerp))

        umbraSlope =
            (sunR - moonR) / g.sunMoonDist

        penumbraSlope =
            (sunR + moonR) / g.sunMoonDist

        moonTop =
            vAdd g.moon (vScale moonR g.axisPerp)

        moonBottom =
            vAdd g.moon (vScale -moonR g.axisPerp)

        penumbraEndW =
            (tEnd - g.penumbraApexLen) * penumbraSlope

        antumbraEndW =
            (tEnd - g.apexLen) * umbraSlope
    in
    [ S.polygon
        [ SA.points (pointsStr [ moonTop, pointAt tEnd penumbraEndW, pointAt tEnd -penumbraEndW, moonBottom ])
        , SA.fill "#9db4c8"
        , SA.opacity "0.45"
        ]
        []
    , S.polygon
        [ SA.points (pointsStr [ g.apex, pointAt tEnd antumbraEndW, pointAt tEnd -antumbraEndW ])
        , SA.fill "#8b5cf6"
        , SA.opacity "0.28"
        ]
        []
    , S.polygon
        [ SA.points (pointsStr [ moonTop, g.apex, moonBottom ])
        , SA.fill "#1e293b"
        , SA.opacity "0.85"
        ]
        []
    ]


axisLine : Geo -> Svg msg
axisLine g =
    let
        tEnd =
            (sceneWidth - sunX) / g.axisDir.x

        end =
            vAdd sunCenter (vScale tEnd g.axisDir)
    in
    S.line
        [ SA.x1 (fs sunCenter.x)
        , SA.y1 (fs sunCenter.y)
        , SA.x2 (fs end.x)
        , SA.y2 (fs end.y)
        , SA.stroke "#94a3b8"
        , SA.strokeWidth "1"
        , SA.strokeDasharray "5 7"
        , SA.opacity "0.7"
        ]
        []


{-| Color the sunlit limb of the Earth by what an observer at each point of
the eclipse path would see: red = total, orange = annular.
-}
viewPathArcs : Geo -> List (Svg msg)
viewPathArcs g =
    List.map
        (\i ->
            let
                a1 =
                    -85 + toFloat i * 5

                p1 =
                    observerPoint a1

                p2 =
                    observerPoint (a1 + 5)

                color =
                    if centralSizeRatio g (a1 + 2.5) >= 1 then
                        "#e11d48"

                    else
                        "#f59e0b"
            in
            S.line
                [ SA.x1 (fs p1.x)
                , SA.y1 (fs p1.y)
                , SA.x2 (fs p2.x)
                , SA.y2 (fs p2.y)
                , SA.stroke color
                , SA.strokeWidth "6"
                , SA.strokeLinecap "round"
                ]
                []
        )
        (List.range 0 33)


apexMarker : Geo -> List (Svg msg)
apexMarker g =
    [ S.circle
        [ SA.cx (fs g.apex.x)
        , SA.cy (fs g.apex.y)
        , SA.r "4"
        , SA.fill "#e11d48"
        , SA.stroke "#ffffff"
        , SA.strokeWidth "1.5"
        ]
        []
    , S.text_
        [ SA.x (fs g.apex.x)
        , SA.y (fs (g.apex.y - 12))
        , SA.textAnchor "middle"
        , SA.fontSize "12"
        , SA.fill "#e11d48"
        , SA.fontWeight "600"
        ]
        [ S.text "Spitze des Kernschattens" ]
    ]


observerMarker : Model -> List (Svg msg)
observerMarker model =
    let
        o =
            observerPoint model.observerAngle
    in
    [ S.circle
        [ SA.cx (fs o.x)
        , SA.cy (fs o.y)
        , SA.r "5"
        , SA.fill "#ffffff"
        , SA.stroke "#111827"
        , SA.strokeWidth "2"
        ]
        []
    , S.text_
        [ SA.x (fs (o.x - 12))
        , SA.y (fs (o.y + 4))
        , SA.textAnchor "end"
        , SA.fontSize "13"
        , SA.fill "#111827"
        , SA.fontWeight "600"
        ]
        [ S.text "Beobachter" ]
    ]


sceneLabels : Geo -> List (Svg msg)
sceneLabels g =
    [ sceneLabel sunX (centerY + sunR + 26) "Sonne"
    , sceneLabel g.moon.x (g.moon.y - moonR - 10) "Mond"
    , sceneLabel earthX (centerY + earthR + 26) "Erde"
    ]


sceneLabel : Float -> Float -> String -> Svg msg
sceneLabel x y label =
    S.text_
        [ SA.x (fs x)
        , SA.y (fs y)
        , SA.textAnchor "middle"
        , SA.fontSize "15"
        , SA.fill "#334155"
        , SA.fontWeight "600"
        ]
        [ S.text label ]



-- LEGEND


viewLegend : EclipseType -> Html Msg
viewLegend eclipse =
    H.div
        [ HA.style "display" "flex"
        , HA.style "flex-wrap" "wrap"
        , HA.style "gap" "14px"
        , HA.style "font-size" "13px"
        , HA.style "color" "#475569"
        , HA.style "margin-top" "8px"
        ]
        ([ legendSwatch "#1e293b" "0.85" "Kernschatten (Umbra)"
         , legendSwatch "#9db4c8" "0.45" "Halbschatten (Penumbra)"
         , legendSwatch "#8b5cf6" "0.28" "Verlängerter Kernschatten (Antumbra)"
         ]
            ++ (if isCentral eclipse then
                    [ legendSwatch "#e11d48" "1" "Pfad: hier total sichtbar"
                    , legendSwatch "#f59e0b" "1" "Pfad: hier ringförmig sichtbar"
                    ]

                else
                    []
               )
        )


legendSwatch : String -> String -> String -> Html msg
legendSwatch color opacity label =
    H.span [ HA.style "display" "inline-flex", HA.style "align-items" "center", HA.style "gap" "6px" ]
        [ H.span
            [ HA.style "display" "inline-block"
            , HA.style "width" "14px"
            , HA.style "height" "14px"
            , HA.style "border-radius" "4px"
            , HA.style "background" color
            , HA.style "opacity" opacity
            ]
            []
        , H.text label
        ]



-- OBSERVER PANEL


viewObserverPanel : Seen -> Html Msg
viewObserverPanel seen =
    let
        ( title, detail ) =
            seenDescription seen
    in
    H.div
        [ HA.style "width" "320px"
        , HA.style "flex-shrink" "0"
        , HA.style "background" "#ffffff"
        , HA.style "border" "1px solid #dbe4ee"
        , HA.style "border-radius" "12px"
        , HA.style "padding" "16px"
        ]
        ([ H.h3 [ HA.style "margin" "0 0 10px", HA.style "font-size" "17px" ]
            [ H.text "Blick des Beobachters" ]
         , S.svg
            [ SA.viewBox "0 0 300 280"
            , HA.style "width" "100%"
            , HA.style "height" "auto"
            ]
            (panelDefs
                :: S.rect
                    [ SA.x "0"
                    , SA.y "0"
                    , SA.width "300"
                    , SA.height "280"
                    , SA.rx "10"
                    , SA.fill skyColor
                    ]
                    []
                :: panelContent seen
            )
         , H.p [ HA.style "margin" "10px 0 0", HA.style "font-size" "15px" ]
            [ H.text "Der Beobachter sieht: "
            , H.strong [] [ H.text title ]
            ]
         , H.p [ HA.style "margin" "4px 0 0", HA.style "font-size" "13px", HA.style "color" "#64748b" ]
            [ H.text detail ]
         ]
            ++ (case ratioOf seen of
                    Just ratio ->
                        [ H.p [ HA.style "margin" "6px 0 0", HA.style "font-size" "13px", HA.style "color" "#64748b" ]
                            [ H.text ("Scheinbarer Monddurchmesser: " ++ String.fromInt (round (ratio * 100)) ++ " % des Sonnendurchmessers") ]
                        ]

                    Nothing ->
                        []
               )
        )


skyColor : String
skyColor =
    "#0b1026"


panelDefs : Svg msg
panelDefs =
    S.defs []
        [ S.radialGradient [ SA.id "sunGradPanel" ]
            [ S.stop [ SA.offset "0%", SA.stopColor "#fff9d6" ] []
            , S.stop [ SA.offset "55%", SA.stopColor "#ffd23f" ] []
            , S.stop [ SA.offset "100%", SA.stopColor "#fb8b24" ] []
            ]
        ]


panelContent : Seen -> List (Svg msg)
panelContent seen =
    let
        cx =
            150

        cy =
            140

        sunPx =
            85

        sunDisc =
            S.circle
                [ SA.cx (fs cx)
                , SA.cy (fs cy)
                , SA.r (fs sunPx)
                , SA.fill "url(#sunGradPanel)"
                ]
                []

        moonDisc mx my r stroke =
            S.circle
                ([ SA.cx (fs mx)
                 , SA.cy (fs my)
                 , SA.r (fs r)
                 , SA.fill skyColor
                 ]
                    ++ (if stroke then
                            [ SA.stroke "#39406b", SA.strokeWidth "1" ]

                        else
                            []
                       )
                )
                []
    in
    case seen of
        SeenTotal ratio ->
            let
                moonPx =
                    sunPx * ratio
            in
            [ sunDisc
            , S.circle [ SA.cx (fs cx), SA.cy (fs cy), SA.r (fs (moonPx + 24)), SA.fill "#ffffff", SA.opacity "0.12" ] []
            , S.circle [ SA.cx (fs cx), SA.cy (fs cy), SA.r (fs (moonPx + 10)), SA.fill "#ffffff", SA.opacity "0.22" ] []
            , moonDisc cx cy moonPx True
            ]

        SeenAnnular ratio ->
            [ sunDisc
            , moonDisc cx cy (sunPx * ratio) False
            ]

        SeenPartial ratio separation ->
            [ sunDisc
            , moonDisc cx (cy + separation * sunPx) (sunPx * ratio) False
            ]

        SeenNothing ->
            [ sunDisc ]


ratioOf : Seen -> Maybe Float
ratioOf seen =
    case seen of
        SeenTotal r ->
            Just r

        SeenAnnular r ->
            Just r

        SeenPartial r _ ->
            Just r

        SeenNothing ->
            Nothing


seenDescription : Seen -> ( String, String )
seenDescription seen =
    case seen of
        SeenTotal _ ->
            ( "eine totale Verfinsterung"
            , "Die Sonne ist vollständig verdeckt – die Korona leuchtet auf."
            )

        SeenAnnular _ ->
            ( "eine ringförmige Verfinsterung"
            , "Ein heller Sonnenring umgibt die dunkle Mondscheibe."
            )

        SeenPartial _ _ ->
            ( "eine partielle Verfinsterung"
            , "Der Mond verdeckt die Sonne nur teilweise."
            )

        SeenNothing ->
            ( "keine Verfinsterung"
            , "Der Mondschatten trifft diesen Ort nicht."
            )



-- EXPLANATION


typeName : EclipseType -> String
typeName t =
    case t of
        Total ->
            "Totale Sonnenfinsternis"

        Annular ->
            "Ringförmige Sonnenfinsternis"

        Hybrid ->
            "Hybride Sonnenfinsternis"

        Partial ->
            "Partielle Sonnenfinsternis"

        NoEclipse ->
            "Keine Sonnenfinsternis"


typeColor : EclipseType -> String
typeColor t =
    case t of
        Total ->
            "#b91c1c"

        Annular ->
            "#d97706"

        Hybrid ->
            "#7c3aed"

        Partial ->
            "#0369a1"

        NoEclipse ->
            "#64748b"


typeExplanation : EclipseType -> List String
typeExplanation t =
    case t of
        Total ->
            [ "Der Mond ist der Erde relativ nah: Sein Kernschatten (Umbra) erreicht die Erdoberfläche. Von dort aus erscheint der Mond größer als die Sonne und verdeckt sie vollständig – nur die Sonnenkorona bleibt als leuchtender Kranz sichtbar."
            , "Wer außerhalb des schmalen Kernschattenpfads im Halbschatten steht, sieht nur eine partielle Finsternis."
            ]

        Annular ->
            [ "Der Mond ist relativ weit von der Erde entfernt: Sein Kernschatten endet schon vor der Erdoberfläche. Die Erde liegt im verlängerten Kernschatten (Antumbra)."
            , "Der Mond erscheint dadurch kleiner als die Sonne und kann sie nicht ganz verdecken – ein heller Sonnenring bleibt sichtbar."
            ]

        Hybrid ->
            [ "Der seltene Grenzfall: Die Spitze des Kernschattens reicht gerade eben bis zur Erde. Weil die Erdoberfläche gekrümmt ist, ist die Mitte des Finsternispfads dem Mond um bis zu einen Erdradius näher als Anfang und Ende des Pfads."
            , "In der Pfadmitte ragt die Erdoberfläche in den Kernschatten hinein – dort ist die Finsternis total. Am Anfang und Ende des Pfads liegt die Oberfläche knapp hinter der Schattenspitze in der Antumbra – dort erscheint sie ringförmig. Ein und dieselbe Finsternis beginnt also ringförmig, wird total und endet wieder ringförmig!"
            , "Der farbige Rand der Erde zeigt, wo auf dem Pfad die Finsternis total (rot) bzw. ringförmig (orange) erscheint. Verschiebe den Regler „Beobachter auf dem Finsternispfad“, um beide Phasen zu sehen."
            ]

        Partial ->
            [ "Kernschatten und Antumbra verfehlen die Erde – nur der Halbschatten (Penumbra) streift sie."
            , "Der Mond schiebt sich von der Erde aus gesehen nur teilweise vor die Sonne. Nirgendwo auf der Erde ist diese Finsternis total oder ringförmig."
            ]

        NoEclipse ->
            [ "Der Schatten des Mondes verfehlt die Erde vollständig – nirgends ist eine Finsternis zu sehen."
            , "Das ist der Normalfall bei Neumond, weil die Mondbahn um etwa 5° gegen die Erdbahn geneigt ist."
            ]


viewExplanation : EclipseType -> Html Msg
viewExplanation t =
    H.div
        [ HA.style "flex" "1"
        , HA.style "min-width" "300px"
        , HA.style "background" "#ffffff"
        , HA.style "border" "1px solid #dbe4ee"
        , HA.style "border-radius" "12px"
        , HA.style "padding" "16px"
        ]
        (H.h3
            [ HA.style "margin" "0 0 10px"
            , HA.style "font-size" "17px"
            , HA.style "color" (typeColor t)
            ]
            [ H.text (typeName t) ]
            :: List.map
                (\paragraph ->
                    H.p
                        [ HA.style "font-size" "15px"
                        , HA.style "line-height" "1.55"
                        , HA.style "margin" "0 0 10px"
                        ]
                        [ H.text paragraph ]
                )
                (typeExplanation t)
        )



-- HELPERS


fs : Float -> String
fs =
    String.fromFloat


pointsStr : List Point -> String
pointsStr points =
    String.join " " (List.map (\p -> fs p.x ++ "," ++ fs p.y) points)
