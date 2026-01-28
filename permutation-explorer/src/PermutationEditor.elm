module PermutationEditor exposing
    ( Model
    , Msg
    , getLabel
    , initFromPermutation
    , permutation
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes exposing (style)
import Permutation
import PermutationView


type Msg
    = SetPermutation Permutation.Permutation


type alias Model =
    { permutation : Permutation.Permutation
    , label : String
    }


{-| Initialize an editor with a specific permutation.
-}
initFromPermutation : String -> Permutation.Permutation -> Model
initFromPermutation label perm =
    { permutation = perm
    , label = label
    }


getLabel : Model -> String
getLabel model =
    model.label


{-| Get the current permutation from the model.
-}
permutation : Model -> Permutation.Permutation
permutation model =
    model.permutation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPermutation perm ->
            ( { model | permutation = perm }, Cmd.none )


{-| View a permutation display with title, characteristics, and graph.
-}
view : Maybe String -> Model -> Html Msg
view edgeColor model =
    PermutationView.viewCard
        [ Html.h2 [ style "margin-top" "0" ] [ Html.text model.label ]
        , PermutationView.viewCharacteristics model.permutation
        , PermutationView.viewGraph edgeColor model.permutation
        ]
