module Effects.App exposing (program)

{-| This module simplifies creating an app using functions that return an
`Effects` wrapper instead of the regular `(Model, Cmd Msg)` pair.

@docs program
-}

import Effects exposing (Effects)
import Html exposing (Html)
import Html.App


{-| This function creates a new app, acting as a thin wrapper around
the `Html.App` function. It will take a list of Cmds returned from your `init` and `update` functions and
batch them together into the single Cmd that the regular Html.App.program functions
expect. This allows you to keep your main module consistent with the rest of your codebase
because the functions can use Effects.
-}
program : { init : Effects model (Cmd msg), update : msg -> model -> Effects model (Cmd msg), subscriptions : model -> Sub msg, view : model -> Html msg } -> Program Never
program { init, update, subscriptions, view } =
    Html.App.program
        { init = init |> Effects.toCmd
        , update = \msg model -> update msg model |> Effects.toCmd
        , subscriptions = subscriptions
        , view = view
        }
