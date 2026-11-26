module Undo exposing (Undoable, advance, advanceUntil, back, canBack)

{-| Linear step history for algorithm steppers.

A model keeps its current `state` plus every earlier state in `past`
(most recent first), so stepping back can go all the way to the start.

-}


type alias Undoable s r =
    { r | state : s, past : List s }


{-| Apply one step, remembering the state before it.
-}
advance : (s -> s) -> Undoable s r -> Undoable s r
advance step m =
    { m | state = step m.state, past = m.state :: m.past }


{-| Step at least once, then keep stepping until `stop` holds,
remembering every intermediate state.
-}
advanceUntil : (s -> Bool) -> (s -> s) -> Undoable s r -> Undoable s r
advanceUntil stop step m =
    let
        m2 =
            advance step m
    in
    if stop m2.state then
        m2

    else
        advanceUntil stop step m2


back : Undoable s r -> Undoable s r
back m =
    case m.past of
        prev :: rest ->
            { m | state = prev, past = rest }

        [] ->
            m


canBack : Undoable s r -> Bool
canBack m =
    not (List.isEmpty m.past)
