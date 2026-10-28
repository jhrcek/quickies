module BoolFunViewTest exposing (suite)

import BoolFun as B
import BoolFun.View as V
import Expect
import Html
import Settings
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "BoolFun.View"
        [ viewImplicantTests
        , boolHelpersTests
        ]


{-| Render an implicant inside a root element, so `Query` can find the
implicant's own top-level node.
-}
renderImplicant : Settings.Settings -> List B.Literal -> Query.Single msg
renderImplicant settings lits =
    Html.div [] [ V.viewImplicant settings (B.implicantFromLiterals (List.length lits) lits) ]
        |> Query.fromHtml


viewImplicantTests : Test
viewImplicantTests =
    describe "viewImplicant"
        [ test "symbols style: a ∧ ¬b" <|
            \_ ->
                renderImplicant Settings.default [ B.Positive, B.Negative, B.DontCare ]
                    |> Query.contains [ Html.span [] [ Html.text "a", Html.text "∧", Html.text "¬b" ] ]
        , test "compact style: a b̄" <|
            \_ ->
                renderImplicant { formula = Settings.Compact } [ B.Positive, B.Negative, B.DontCare ]
                    |> Query.contains [ Html.span [] [ Html.text "a", Settings.overbar "b" ] ]
        , test "don't-cares are left out" <|
            \_ ->
                renderImplicant Settings.default [ B.DontCare, B.Negative, B.DontCare, B.Positive ]
                    |> Query.contains [ Html.span [] [ Html.text "¬b", Html.text "∧", Html.text "d" ] ]
        , test "the empty implicant is ⊤ in both styles" <|
            \_ ->
                [ Settings.default, { formula = Settings.Compact } ]
                    |> List.map (\settings -> renderImplicant settings [ B.DontCare, B.DontCare ])
                    |> List.map (\q () -> Query.has [ Selector.text "⊤" ] q)
                    |> (\expectations -> Expect.all expectations ())
        ]


boolHelpersTests : Test
boolHelpersTests =
    describe "showBool / boolColor"
        [ test "showBool spells out the value" <|
            \_ ->
                List.map V.showBool [ True, False ]
                    |> Expect.equalLists [ "True", "False" ]
        , test "boolColor differs for True and False" <|
            \_ ->
                V.boolColor True
                    |> Expect.notEqual (V.boolColor False)
        ]
