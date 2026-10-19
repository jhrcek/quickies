port module Ports exposing (storeCustomCategories, storeCustomFunctors)

{-| The app's only side effects besides navigation: custom categories and custom
functors are kept in the browser's local storage (see `index.html`, which also hands them
to `Main` as flags).
-}


{-| Store the JSON-encoded custom categories (see `Math.CustomCategory.encode`).
-}
port storeCustomCategories : String -> Cmd msg


{-| Store the JSON-encoded custom functors (see `Math.CustomFunctor.encode`).
-}
port storeCustomFunctors : String -> Cmd msg
