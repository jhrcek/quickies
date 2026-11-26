module Journey exposing (Info, Step(..), all, index, info, next, prev)

{-| The ordered list of lessons.

To add a lesson: add a constructor to `Step`, then follow the compiler.
`index` and `info` are exhaustive `case` expressions, so they cannot be
forgotten; the test suite checks that `all` lists every step in index order.

-}


type Step
    = Welcome
    | Literals
    | Clauses
    | Formulas
    | BruteForce
    | Backtracking
    | UnitPropagation
    | Dpll
    | Cdcl
    | LocalSearch
    | PhaseTransition
    | Reductions
    | Beyond


all : List Step
all =
    [ Welcome, Literals, Clauses, Formulas, BruteForce, Backtracking, UnitPropagation, Dpll, Cdcl, LocalSearch, PhaseTransition, Reductions, Beyond ]


index : Step -> Int
index s =
    case s of
        Welcome ->
            0

        Literals ->
            1

        Clauses ->
            2

        Formulas ->
            3

        BruteForce ->
            4

        Backtracking ->
            5

        UnitPropagation ->
            6

        Dpll ->
            7

        Cdcl ->
            8

        LocalSearch ->
            9

        PhaseTransition ->
            10

        Reductions ->
            11

        Beyond ->
            12


type alias Info =
    { title : String
    , short : String
    , slug : String
    }


info : Step -> Info
info s =
    case s of
        Welcome ->
            Info "What is 3-SAT?" "Start" "start"

        Literals ->
            Info "Variables and literals" "Literals" "literals"

        Clauses ->
            Info "Clauses: little vetoes" "Clauses" "clauses"

        Formulas ->
            Info "Formulas: satisfy them all" "Formulas" "formulas"

        BruteForce ->
            Info "Brute force and the wall of 2ⁿ" "Brute force" "brute-force"

        Backtracking ->
            Info "Backtracking: prune early" "Backtracking" "backtracking"

        UnitPropagation ->
            Info "Unit propagation: forced moves" "Propagation" "propagation"

        Dpll ->
            Info "DPLL: search plus propagation" "DPLL" "dpll"

        Cdcl ->
            Info "CDCL: learning from conflicts" "CDCL" "cdcl"

        LocalSearch ->
            Info "Local search: WalkSAT" "WalkSAT" "walksat"

        PhaseTransition ->
            Info "The phase transition" "Phase transition" "phase"

        Reductions ->
            Info "Everything becomes SAT" "Reductions" "reductions"

        Beyond ->
            Info "Inside real solvers" "Beyond" "beyond"


fromIndex : Int -> Maybe Step
fromIndex i =
    List.drop i all |> List.head


next : Step -> Maybe Step
next s =
    fromIndex (index s + 1)


prev : Step -> Maybe Step
prev s =
    if index s == 0 then
        Nothing

    else
        fromIndex (index s - 1)
