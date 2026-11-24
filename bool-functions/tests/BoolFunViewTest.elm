module BoolFunViewTest exposing (suite)

import BoolFun as B
import BoolFun.View as V
import Expect
import Settings
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "BoolFun.View"
        [ viewImplicantTests
        , viewClauseTests
        , boolHelpersTests
        , viewAlgebraicNormalFormTests
        ]


implicantTex : Settings.Settings -> List B.Literal -> String
implicantTex settings lits =
    V.implicantTex settings (B.implicantFromLiterals (List.length lits) lits)


viewImplicantTests : Test
viewImplicantTests =
    describe "implicantTex"
        [ test "symbols style: a ∧ ¬b" <|
            \_ ->
                implicantTex Settings.default [ B.Positive, B.Negative, B.DontCare ]
                    |> Expect.equal "a \\land \\neg b"
        , test "compact style: a b̄" <|
            \_ ->
                implicantTex { formula = Settings.Compact, bool = Settings.Words } [ B.Positive, B.Negative, B.DontCare ]
                    |> Expect.equal "a\\overline{b}"
        , test "don't-cares are left out" <|
            \_ ->
                implicantTex Settings.default [ B.DontCare, B.Negative, B.DontCare, B.Positive ]
                    |> Expect.equal "\\neg b \\land d"
        , test "the empty implicant is ⊤ in both styles" <|
            \_ ->
                [ Settings.default, { formula = Settings.Compact, bool = Settings.Words } ]
                    |> List.map (\settings -> implicantTex settings [ B.DontCare, B.DontCare ])
                    |> Expect.equalLists [ "\\top", "\\top" ]
        ]


viewClauseTests : Test
viewClauseTests =
    describe "clauseTex"
        [ test "symbols style: a ∨ ¬b" <|
            \_ ->
                V.clauseTex Settings.default [ B.Positive, B.Negative, B.DontCare ]
                    |> Expect.equal "a \\lor \\neg b"
        , test "compact style: a + b̄" <|
            \_ ->
                V.clauseTex { formula = Settings.Compact, bool = Settings.Words } [ B.Positive, B.Negative, B.DontCare ]
                    |> Expect.equal "a + \\overline{b}"
        , test "don't-cares are left out" <|
            \_ ->
                V.clauseTex Settings.default [ B.DontCare, B.Negative, B.DontCare, B.Positive ]
                    |> Expect.equal "\\neg b \\lor d"
        , test "the empty clause is ⊥ in both styles" <|
            \_ ->
                [ Settings.default, { formula = Settings.Compact, bool = Settings.Words } ]
                    |> List.map (\settings -> V.clauseTex settings [ B.DontCare, B.DontCare ])
                    |> Expect.equalLists [ "\\bot", "\\bot" ]
        ]


boolHelpersTests : Test
boolHelpersTests =
    describe "showBool / boolColor"
        [ test "showBool respects the boolean display style" <|
            \_ ->
                [ Settings.Words, Settings.Digits, Settings.Marks, Settings.ColorOnly ]
                    |> List.map
                        (\style ->
                            List.map (Settings.showBool { formula = Settings.Symbols, bool = style }) [ True, False ]
                        )
                    |> Expect.equalLists [ [ "True", "False" ], [ "1", "0" ], [ "✓", "✗" ], [ "", "" ] ]
        , test "showBoolInFormula falls back to 0 / 1 for color only" <|
            \_ ->
                List.map (Settings.showBoolInFormula { formula = Settings.Symbols, bool = Settings.ColorOnly }) [ True, False ]
                    |> Expect.equalLists [ "1", "0" ]
        , test "boolColor differs for True and False" <|
            \_ ->
                V.boolColor True
                    |> Expect.notEqual (V.boolColor False)
        ]


viewAlgebraicNormalFormTests : Test
viewAlgebraicNormalFormTests =
    describe "algebraicNormalFormTex"
        [ test "the empty polynomial is 0" <|
            \_ ->
                V.algebraicNormalFormTex Settings.default 2 []
                    |> Expect.equal "0"
        , test "symbols style: 1 ⊕ a ⊕ b∧c" <|
            \_ ->
                V.algebraicNormalFormTex Settings.default 3 [ 0, 4, 3 ]
                    |> Expect.equal "1 \\oplus a \\oplus b \\land c"
        , test "compact style: 1 ⊕ a ⊕ bc" <|
            \_ ->
                V.algebraicNormalFormTex { formula = Settings.Compact, bool = Settings.Words } 3 [ 0, 4, 3 ]
                    |> Expect.equal "1 \\oplus a \\oplus bc"
        ]
