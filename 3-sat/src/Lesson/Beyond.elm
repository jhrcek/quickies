module Lesson.Beyond exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Ui exposing (em, insight, p, strong, t, wiki)


item : String -> List (Html msg) -> Html msg
item title body =
    div [ class "panel card" ] [ Ui.h3 title, p body ]


view : Html msg
view =
    div []
        [ p [ t "The solvers in this app are the real algorithms, written for clarity. Production solvers such as MiniSat, Glucose, CaDiCaL and Kissat run the same CDCL loop, plus a handful of ideas that each bought orders of magnitude:" ]
        , item "Watched literals" [ t "Propagation without scanning every clause: each clause watches two literals and is only visited when one of them becomes false. Backtracking costs nothing, because watches stay valid. Introduced by the ", wiki "Chaff algorithm" "Chaff", t " solver (2001)." ]
        , item "VSIDS and phase saving" [ t "Our solvers always decide the lowest-numbered variable. Real ones keep an activity score per variable, bump the variables that appear in recent conflicts, and decay all scores over time — so the search focuses on the current trouble spot. When a variable is re-decided, it gets the value it had last time (phase saving), which keeps good partial solutions alive across backjumps." ]
        , item "Restarts" [ t "Every so often, throw away the whole trail but keep the learned clauses and the scores. It sounds wasteful, yet it escapes bad early decisions, and the learned clauses make the next descent smarter. Schedules follow the Luby sequence or adapt to clause quality." ]
        , item "Clause deletion" [ t "Millions of learned clauses would make propagation crawl. Solvers periodically delete the least useful ones. Glucose's insight: rate a learned clause by its LBD, the number of distinct decision levels among its literals. Low-LBD (“glue”) clauses tend to be useful forever." ]
        , item "Pre- and inprocessing" [ t "Before and during search, simplify the formula: eliminate variables by resolution when that doesn't grow it, remove subsumed clauses, detect equivalent literals. Often the formula shrinks a lot before any decision is made." ]
        , item "Proofs you can check" [ t "When a solver says UNSAT, why trust it? Modern solvers emit a DRAT proof, essentially the sequence of learned clauses, that a small independent checker verifies. The SAT Competition requires them." ]
        , item "Beyond plain SAT" [ wiki "Satisfiability modulo theories" "SMT solvers", t " (Z3, cvc5) put a CDCL engine in charge and plug in theories like arithmetic and arrays — they power program verifiers and cloud policy checkers. ", wiki "Maximum satisfiability problem" "MaxSAT", t " optimises the number of satisfied clauses, ", wiki "Sharp-SAT" "#SAT", t " counts solutions, and incremental SAT answers thousands of related queries quickly." ]
        , insight "Where to read more"
            [ p
                [ t "Donald Knuth, "
                , Html.em [] [ wiki "The Art of Computer Programming" "The Art of Computer Programming" ]
                , t ", Volume 4B, the long chapter on Satisfiability (a joy, full of exercises). The "
                , em "Handbook of Satisfiability"
                , t " (Biere, Heule, van Maaren, Walsh) for the full theory. And Eén & Sörensson's short paper “An Extensible SAT-solver”, which describes MiniSat in about 500 lines of C++ — a perfect next step after this app."
                ]
            ]
        , insight "Extending this app"
            [ p
                [ t "The solvers live in pure modules under "
                , strong "src/Sat"
                , t " and are property-tested against brute force. Lessons are self-contained modules under "
                , strong "src/Lesson"
                , t "; the list of steps lives in "
                , strong "src/Journey.elm"
                , t ". Adding a constructor there makes the compiler point at every place that needs updating."
                ]
            ]
        ]
