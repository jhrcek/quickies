port module Ports exposing (storeCustomCategories)

{-| The app's only side effect besides navigation: custom categories are kept in the
browser's local storage (see `index.html`, which also hands them to `Main` as flags).
-}


{-| Store the JSON-encoded custom categories (see `Math.CustomCategory.encode`).
-}
port storeCustomCategories : String -> Cmd msg
