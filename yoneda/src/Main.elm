module Main exposing (main)

{-| Road to Yoneda: an interactive, chapter-by-chapter explainer.

Chapters are routed via the URL hash (see `Route`); the global composition-order setting
is plain model state. Each chapter is its own `Page.*` module with local state, and
mirrors its main selections into the hash query (`#/sets?a=3...`) so that every
interactive state has a link. Navigation links carry the target chapter's current state.

Playgrounds (`Route.Playground`) replace the whole layout with a full-viewport view whose
close button leads back to the last chapter visited.

Custom categories built in the category playground live here (`customs`), are offered by
the category pickers of the chapters, and are kept in local storage: `index.html` passes
the stored JSON as flags and writes it back through `Ports.storeCustomCategories`. Custom
functors built in the functor playground (`customFunctors`) are kept the same way
(`Ports.storeCustomFunctors`); they refer to their category by name.

-}

import Browser
import Browser.Navigation as Navigation
import Dict
import Html exposing (Html, a, div, h1, h2, label, nav, span, text)
import Html.Attributes exposing (attribute, class, classList, href, title)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Math.Categories as Categories exposing (Example)
import Math.CustomCategory as CustomCategory
import Math.CustomFunctor as CustomFunctor
import Math.SetFunctor exposing (SetFunctor)
import Math.Setting as Setting
import Page.Categories
import Page.Cayley
import Page.Functors
import Page.Glossary
import Page.Groups
import Page.HomFunctors
import Page.Intro
import Page.NaturalTransformations
import Page.Playground.Category
import Page.Playground.Functor
import Page.Playground.HomFunctors
import Page.Sets
import Page.YonedaEmbedding
import Page.YonedaLemma
import Ports
import Query exposing (Query)
import Route exposing (Chapter(..), Playground(..))
import Url exposing (Url)
import View.Notation as Notation exposing (CompositionOrder(..))


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Navigation.Key
    , chapter : Chapter
    , returnTo : Chapter
    , order : CompositionOrder
    , customs : List Example -- saved custom categories, in the order they were first saved
    , customFunctors : List CustomFunctor.Saved -- saved custom functors, in the order they were first saved
    , sets : Page.Sets.Model
    , groups : Page.Groups.Model
    , cayley : Page.Cayley.Model
    , categories : Page.Categories.Model
    , functors : Page.Functors.Model
    , homFunctors : Page.HomFunctors.Model
    , naturalTransformations : Page.NaturalTransformations.Model
    , yonedaLemma : Page.YonedaLemma.Model
    , yonedaEmbedding : Page.YonedaEmbedding.Model
    , categoryPlayground : Page.Playground.Category.Model
    , functorPlayground : Page.Playground.Functor.Model
    , homFunctorsPlayground : Page.Playground.HomFunctors.Model
    }


init : Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( chapter, query ) =
            Route.parse url
    in
    { key = key
    , chapter = chapter
    , returnTo = Intro
    , order = Diagrammatic
    , customs = decodeCustoms flags
    , customFunctors = decodeCustomFunctors flags
    , sets = Page.Sets.init
    , groups = Page.Groups.init
    , cayley = Page.Cayley.init
    , categories = Page.Categories.init
    , functors = Page.Functors.init
    , homFunctors = Page.HomFunctors.init
    , naturalTransformations = Page.NaturalTransformations.init
    , yonedaLemma = Page.YonedaLemma.init
    , yonedaEmbedding = Page.YonedaEmbedding.init
    , categoryPlayground = Page.Playground.Category.init
    , functorPlayground = Page.Playground.Functor.init
    , homFunctorsPlayground = Page.Playground.HomFunctors.init
    }
        |> rememberReturn
        |> applyQuery query
        |> syncUrl


{-| The custom categories stored in local storage (a JSON string, or null). Entries that
fail to decode or are not categories are dropped.
-}
decodeCustoms : Decode.Value -> List Example
decodeCustoms flags =
    case Decode.decodeValue (Decode.field "categories" (Decode.nullable Decode.string)) flags of
        Ok (Just json) ->
            Decode.decodeString CustomCategory.decoder json
                |> Result.withDefault []
                |> List.filterMap CustomCategory.toCategory
                |> List.map Categories.layoutFor

        _ ->
            []


{-| The custom functors stored in local storage (a JSON string, or null). They are kept
even when they no longer fit their category, so that a functor survives its category
being deleted and saved again.
-}
decodeCustomFunctors : Decode.Value -> List CustomFunctor.Saved
decodeCustomFunctors flags =
    case Decode.decodeValue (Decode.field "functors" (Decode.nullable Decode.string)) flags of
        Ok (Just json) ->
            Decode.decodeString CustomFunctor.decoder json
                |> Result.withDefault []

        _ ->
            []


storeCustoms : List Example -> Cmd msg
storeCustoms customs =
    customs
        |> List.map (.category >> CustomCategory.fromCategory)
        |> CustomCategory.encode
        |> Encode.encode 0
        |> Ports.storeCustomCategories


{-| Apply a change to the saved custom categories: store them, and re-apply its own deep
link to every page showing the category of that name or one of the reader's functors on
it, so that it picks up the new version (or falls back to its default when the category
was deleted).
-}
applyEffect : Page.Playground.Category.Effect -> Model -> ( Model, Cmd Msg )
applyEffect effect model =
    let
        ( customs, name ) =
            case effect of
                Page.Playground.Category.Store cat ->
                    let
                        ex =
                            Categories.layoutFor cat
                    in
                    ( if List.any (\c -> c.category.name == cat.name) model.customs then
                        List.map
                            (\c ->
                                if c.category.name == cat.name then
                                    ex

                                else
                                    c
                            )
                            model.customs

                      else
                        model.customs ++ [ ex ]
                    , cat.name
                    )

                Page.Playground.Category.Delete n ->
                    ( List.filter (\c -> c.category.name /= n) model.customs, n )

        updated =
            { model | customs = customs }

        names =
            name :: (model.customFunctors |> List.filter (\s -> s.category == name) |> List.map .name)

        reload =
            reloadIfShowing names

        withFunctors =
            reloadFunctorPages names updated
    in
    ( { withFunctors
        | categories = reload Page.Categories.toQuery (Page.Categories.fromQuery customs) Page.Categories.init model.categories
        , homFunctors = reload Page.HomFunctors.toQuery (Page.HomFunctors.fromQuery customs) Page.HomFunctors.init model.homFunctors
        , yonedaEmbedding = reload Page.YonedaEmbedding.toQuery (Page.YonedaEmbedding.fromQuery customs) Page.YonedaEmbedding.init model.yonedaEmbedding
        , homFunctorsPlayground = reload Page.Playground.HomFunctors.toQuery (Page.Playground.HomFunctors.fromQuery customs) Page.Playground.HomFunctors.init model.homFunctorsPlayground
        , functorPlayground = Page.Playground.Functor.categoryChanged customs name model.functorPlayground
      }
    , storeCustoms customs
    )


{-| Apply a change to the saved custom functors: store them, and re-apply its own deep link
to every page that offers the reader's functors and shows that functor or its category.
-}
applyFunctorEffect : Page.Playground.Functor.Effect -> Model -> ( Model, Cmd Msg )
applyFunctorEffect effect model =
    let
        ( customFunctors, names ) =
            case effect of
                Page.Playground.Functor.Store saved ->
                    ( if List.any (\s -> s.name == saved.name) model.customFunctors then
                        List.map
                            (\s ->
                                if s.name == saved.name then
                                    saved

                                else
                                    s
                            )
                            model.customFunctors

                      else
                        model.customFunctors ++ [ saved ]
                    , saved.name :: saved.category :: categoryOf saved.name
                    )

                Page.Playground.Functor.Delete name ->
                    ( List.filter (\s -> s.name /= name) model.customFunctors
                    , name :: categoryOf name
                    )

        categoryOf name =
            -- the category the functor was on before the change
            model.customFunctors |> List.filter (\s -> s.name == name) |> List.map .category
    in
    ( reloadFunctorPages names { model | customFunctors = customFunctors }
    , customFunctors
        |> CustomFunctor.encode
        |> Encode.encode 0
        |> Ports.storeCustomFunctors
    )


{-| Re-apply its own deep link to a page whose deep link mentions one of the names.
-}
reloadIfShowing : List String -> (page -> List ( String, String )) -> (Query -> page -> page) -> page -> page -> page
reloadIfShowing names toQuery fromQuery initial page =
    let
        params =
            toQuery page
    in
    if List.any (\( _, v ) -> List.member v names) params then
        fromQuery (Query.fromString (Query.toString params)) initial

    else
        page


{-| `reloadIfShowing` for the pages offering the reader's functors, with the custom
categories and functors of the given model.
-}
reloadFunctorPages : List String -> Model -> Model
reloadFunctorPages names model =
    let
        reload =
            reloadIfShowing names

        yours =
            yourFunctors model
    in
    { model
        | functors = reload Page.Functors.toQuery (Page.Functors.fromQuery model.customs yours) Page.Functors.init model.functors
        , naturalTransformations = reload Page.NaturalTransformations.toQuery (Page.NaturalTransformations.fromQuery model.customs yours) Page.NaturalTransformations.init model.naturalTransformations
        , yonedaLemma = reload Page.YonedaLemma.toQuery (Page.YonedaLemma.fromQuery model.customs yours) Page.YonedaLemma.init model.yonedaLemma
    }


{-| The reader's saved functors that still are functors on their category.
-}
yourFunctors : Model -> List SetFunctor
yourFunctors model =
    Setting.resolveFunctors model.customs model.customFunctors


{-| Remember the current chapter as the place a playground's close button returns to.
-}
rememberReturn : Model -> Model
rememberReturn model =
    case model.chapter of
        Playground _ ->
            model

        c ->
            { model | returnTo = c }


{-| Apply a deep-link query to the current chapter's page.
-}
applyQuery : Query -> Model -> Model
applyQuery query model =
    case model.chapter of
        Intro ->
            model

        Glossary ->
            model

        Playground CategoryPlayground ->
            { model | categoryPlayground = Page.Playground.Category.fromQuery model.customs query model.categoryPlayground }

        Playground FunctorPlayground ->
            { model | functorPlayground = Page.Playground.Functor.fromQuery model.customs model.customFunctors query model.functorPlayground }

        Playground HomFunctorsPlayground ->
            { model | homFunctorsPlayground = Page.Playground.HomFunctors.fromQuery model.customs query model.homFunctorsPlayground }

        Sets ->
            { model | sets = Page.Sets.fromQuery query model.sets }

        Groups ->
            { model | groups = Page.Groups.fromQuery query model.groups }

        Cayley ->
            { model | cayley = Page.Cayley.fromQuery query model.cayley }

        Categories ->
            { model | categories = Page.Categories.fromQuery model.customs query model.categories }

        Functors ->
            { model | functors = Page.Functors.fromQuery model.customs (yourFunctors model) query model.functors }

        HomFunctors ->
            { model | homFunctors = Page.HomFunctors.fromQuery model.customs query model.homFunctors }

        NaturalTransformations ->
            { model | naturalTransformations = Page.NaturalTransformations.fromQuery model.customs (yourFunctors model) query model.naturalTransformations }

        YonedaLemma ->
            { model | yonedaLemma = Page.YonedaLemma.fromQuery model.customs (yourFunctors model) query model.yonedaLemma }

        YonedaEmbedding ->
            { model | yonedaEmbedding = Page.YonedaEmbedding.fromQuery model.customs query model.yonedaEmbedding }


{-| The deep-link state of a chapter's page.
-}
queryFor : Chapter -> Model -> List ( String, String )
queryFor chapter model =
    case chapter of
        Intro ->
            []

        Glossary ->
            []

        Playground CategoryPlayground ->
            []

        Playground FunctorPlayground ->
            Page.Playground.Functor.toQuery model.functorPlayground

        Playground HomFunctorsPlayground ->
            Page.Playground.HomFunctors.toQuery model.homFunctorsPlayground

        Sets ->
            Page.Sets.toQuery model.sets

        Groups ->
            Page.Groups.toQuery model.groups

        Cayley ->
            Page.Cayley.toQuery model.cayley

        Categories ->
            Page.Categories.toQuery model.categories

        Functors ->
            Page.Functors.toQuery model.functors

        HomFunctors ->
            Page.HomFunctors.toQuery model.homFunctors

        NaturalTransformations ->
            Page.NaturalTransformations.toQuery model.naturalTransformations

        YonedaLemma ->
            Page.YonedaLemma.toQuery model.yonedaLemma

        YonedaEmbedding ->
            Page.YonedaEmbedding.toQuery model.yonedaEmbedding


{-| A link to a chapter that carries the chapter's current state.
-}
linkTo : Chapter -> Model -> String
linkTo chapter model =
    Route.toStringWith chapter (queryFor chapter model)


{-| Replace the URL by the deep link of the current state (no history entry).
-}
syncUrl : Model -> ( Model, Cmd Msg )
syncUrl model =
    ( model, Navigation.replaceUrl model.key (linkTo model.chapter model) )


{-| After a page message: replace the URL only if the page's deep-link state changed, so
that hover highlights and other transient UI state do not hit the browser's history API
(Safari throws after 100 `replaceState` calls in 30 seconds).
-}
syncIfChanged : Model -> Model -> ( Model, Cmd Msg )
syncIfChanged old new =
    if queryFor new.chapter new == queryFor old.chapter old then
        ( new, Cmd.none )

    else
        syncUrl new



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | SetOrder CompositionOrder
    | SetsMsg Page.Sets.Msg
    | GroupsMsg Page.Groups.Msg
    | CayleyMsg Page.Cayley.Msg
    | CategoriesMsg Page.Categories.Msg
    | FunctorsMsg Page.Functors.Msg
    | HomFunctorsMsg Page.HomFunctors.Msg
    | NaturalTransformationsMsg Page.NaturalTransformations.Msg
    | YonedaLemmaMsg Page.YonedaLemma.Msg
    | YonedaEmbeddingMsg Page.YonedaEmbedding.Msg
    | CategoryPlaygroundMsg Page.Playground.Category.Msg
    | FunctorPlaygroundMsg Page.Playground.Functor.Msg
    | HomFunctorsPlaygroundMsg Page.Playground.HomFunctors.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        UrlRequested (Browser.External href) ->
            ( model, Navigation.load href )

        UrlChanged url ->
            let
                ( chapter, query ) =
                    Route.parse url

                moved =
                    rememberReturn { model | chapter = chapter }
            in
            if chapter == model.chapter && Dict.toList query == Dict.toList (Query.fromString (Query.toString (queryFor chapter model))) then
                -- our own `replaceUrl` echoing back: nothing to apply
                ( moved, Cmd.none )

            else
                moved |> applyQuery query |> syncUrl

        SetOrder order ->
            ( { model | order = order }, Cmd.none )

        SetsMsg m ->
            syncIfChanged model { model | sets = Page.Sets.update m model.sets }

        GroupsMsg m ->
            syncIfChanged model { model | groups = Page.Groups.update m model.groups }

        CayleyMsg m ->
            syncIfChanged model { model | cayley = Page.Cayley.update m model.cayley }

        CategoriesMsg m ->
            syncIfChanged model { model | categories = Page.Categories.update m model.categories }

        FunctorsMsg m ->
            syncIfChanged model { model | functors = Page.Functors.update m model.functors }

        HomFunctorsMsg m ->
            syncIfChanged model { model | homFunctors = Page.HomFunctors.update m model.homFunctors }

        NaturalTransformationsMsg m ->
            syncIfChanged model { model | naturalTransformations = Page.NaturalTransformations.update m model.naturalTransformations }

        YonedaLemmaMsg m ->
            syncIfChanged model { model | yonedaLemma = Page.YonedaLemma.update m model.yonedaLemma }

        YonedaEmbeddingMsg m ->
            syncIfChanged model { model | yonedaEmbedding = Page.YonedaEmbedding.update m model.yonedaEmbedding }

        CategoryPlaygroundMsg m ->
            let
                ( playground, effect ) =
                    Page.Playground.Category.update model.customs m model.categoryPlayground

                updated =
                    { model | categoryPlayground = playground }
            in
            case effect of
                Just e ->
                    applyEffect e updated

                Nothing ->
                    ( updated, Cmd.none )

        FunctorPlaygroundMsg m ->
            let
                ( playground, effect ) =
                    Page.Playground.Functor.update model.customs model.customFunctors m model.functorPlayground

                ( updated, cmd ) =
                    syncIfChanged model { model | functorPlayground = playground }
            in
            case effect of
                Just e ->
                    applyFunctorEffect e updated |> Tuple.mapSecond (\store -> Cmd.batch [ cmd, store ])

                Nothing ->
                    ( updated, cmd )

        HomFunctorsPlaygroundMsg m ->
            syncIfChanged model { model | homFunctorsPlayground = Page.Playground.HomFunctors.update model.customs m model.homFunctorsPlayground }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = Route.chapterTitle model.chapter ++ " · Road to Yoneda"
    , body =
        [ case model.chapter of
            Playground p ->
                viewPlayground model p

            _ ->
                div [ class "app" ]
                    [ sidebar model
                    , div [ class "main" ]
                        [ topbar model
                        , div [ class "content" ]
                            [ viewChapter model
                            , prevNext model
                            ]
                        ]
                    ]
        ]
    }


{-| A playground fills the viewport: a slim title bar with a close button, and the
playground's own body below it.
-}
viewPlayground : Model -> Playground -> Html Msg
viewPlayground model playground =
    div [ class "playground" ]
        [ div [ class "playground-titlebar" ]
            [ h2 [] [ text (Route.playgroundTitle playground) ]
            , a
                [ class "playground-close"
                , href (linkTo model.returnTo model)
                , title "Close playground"
                , attribute "aria-label" "Close playground"
                ]
                [ text "×" ]
            ]
        , div [ class "playground-body" ]
            [ case playground of
                CategoryPlayground ->
                    Html.map CategoryPlaygroundMsg (Page.Playground.Category.view model.customs model.categoryPlayground)

                FunctorPlayground ->
                    Html.map FunctorPlaygroundMsg (Page.Playground.Functor.view model.customs model.customFunctors model.functorPlayground)

                HomFunctorsPlayground ->
                    Html.map HomFunctorsPlaygroundMsg (Page.Playground.HomFunctors.view model.customs model.homFunctorsPlayground)
            ]
        ]


sidebar : Model -> Html Msg
sidebar model =
    nav [ class "sidebar" ]
        (h1 [] [ text "Road to Yoneda" ]
            :: List.map
                (\c ->
                    a
                        [ href (linkTo c model)
                        , classList [ ( "active", c == model.chapter ) ]
                        ]
                        [ span [ class "num" ]
                            [ text
                                (if c == Intro then
                                    ""

                                 else
                                    String.fromInt (Route.chapterNumber c)
                                )
                            ]
                        , text (Route.chapterTitle c)
                        ]
                )
                Route.allChapters
            ++ [ a
                    [ href (Route.toString Glossary)
                    , classList [ ( "active", model.chapter == Glossary ), ( "glossary", True ) ]
                    ]
                    [ span [ class "num" ] [ text "" ], text (Route.chapterTitle Glossary) ]
               , div [ class "sidebar-section" ] [ text "Playgrounds" ]
               ]
            ++ List.map
                (\p ->
                    a [ href (Route.toString (Playground p)) ]
                        [ span [ class "num" ] [ text "" ], text (Route.playgroundTitle p) ]
                )
                Route.allPlaygrounds
        )


topbar : Model -> Html Msg
topbar model =
    div [ class "topbar" ]
        [ span [ class "muted" ]
            [ text
                (case model.chapter of
                    Intro ->
                        Route.chapterTitle Intro

                    Glossary ->
                        Route.chapterTitle Glossary

                    Playground p ->
                        Route.playgroundTitle p

                    c ->
                        "Chapter " ++ String.fromInt (Route.chapterNumber c) ++ ": " ++ Route.chapterTitle c
                )
            ]
        , div [ class "controls" ]
            [ label [ title (Notation.orderSymbolExplanation model.order) ] [ text "Composition order:" ]
            , orderButton model.order Diagrammatic
            , orderButton model.order Classical
            ]
        ]


orderButton : CompositionOrder -> CompositionOrder -> Html Msg
orderButton current order =
    Html.button
        [ classList [ ( "active", current == order ) ]
        , onClick (SetOrder order)
        , title (Notation.orderSymbolExplanation order)
        ]
        [ text (Notation.orderName order) ]


viewChapter : Model -> Html Msg
viewChapter model =
    case model.chapter of
        Intro ->
            Page.Intro.view model.order

        Glossary ->
            Page.Glossary.view model.order

        Playground _ ->
            text ""

        Sets ->
            Html.map SetsMsg (Page.Sets.view model.order model.sets)

        Groups ->
            Html.map GroupsMsg (Page.Groups.view model.groups)

        Cayley ->
            Html.map CayleyMsg (Page.Cayley.view model.order model.cayley)

        Categories ->
            Html.map CategoriesMsg (Page.Categories.view model.order model.customs model.categories)

        Functors ->
            Html.map FunctorsMsg (Page.Functors.view model.order model.customs (yourFunctors model) model.functors)

        HomFunctors ->
            Html.map HomFunctorsMsg (Page.HomFunctors.view model.order model.customs model.homFunctors)

        NaturalTransformations ->
            Html.map NaturalTransformationsMsg (Page.NaturalTransformations.view model.order model.customs (yourFunctors model) model.naturalTransformations)

        YonedaLemma ->
            Html.map YonedaLemmaMsg (Page.YonedaLemma.view model.order model.customs (yourFunctors model) model.yonedaLemma)

        YonedaEmbedding ->
            Html.map YonedaEmbeddingMsg (Page.YonedaEmbedding.view model.order model.customs model.yonedaEmbedding)


prevNext : Model -> Html Msg
prevNext model =
    let
        link prefix c =
            a [ href (linkTo c model) ] [ text (prefix ++ Route.chapterTitle c) ]
    in
    div [ class "prevnext" ]
        [ case Route.previous model.chapter of
            Just c ->
                link "← " c

            Nothing ->
                span [] []
        , case Route.next model.chapter of
            Just c ->
                link "Next: " c

            Nothing ->
                span [] []
        ]
