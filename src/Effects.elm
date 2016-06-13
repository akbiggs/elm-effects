module Effects
    exposing
        ( Effects
        , Handler
        , None
        , init
        , return
        , getValue
        , getEffects
        , add
        , addIf
        , mapOverValue
        , mapOverEffects
        , handle
        , ignoreUnused
        , andThen
        , batch
        , toCmd
        )

{-| This package contains several useful functions for operating on pairs
of a value and a list of side-effects. Side-effects do not have to be external
Elm events - they can be internal app events, such as a message indicating that
the score of your game should increase, or that you want to navigate the app
back to the home page.

More documentation coming soon!

# Types
@docs Effects, None, Handler

# Constructors
@docs init, return

# Getters
@docs getValue, getEffects

# Adding Effects
@docs add, addIf

# Mapping
@docs mapOverValue, mapOverEffects

# Handling Effects
@docs handle, ignoreUnused

# Chaining Operations
@docs andThen

# Combining
@docs batch

# Conversion
@docs toCmd
-}


{-| A value combining a value and a list of effects associated with that current
value, e.g. resulting from an update or initialization.

For example, having the player jump might update the state of the player and have a
side-effect of adding some dust particles into the scene.
-}
type alias Effects a effect =
    ( a, List effect )


{-| `None` is useful when updating a component shouldn't have any side effects on
the world. This way, you can still keep the structure of your component the same,
making your code consistent and flexible.
-}
type alias None =
    ()


{-| A `Handler` is a function that takes an effect and applies it to an object,
giving back the new state of that object and any resulting effects. This is useful
when you want to take a child's side-effect and have the parent react to it. For example,
if updating the child has an effect of `IncreaseScore 100`, the parent might update its
`score` value by that amount and return an additional side-effect of `PlaySound "scoreIncreased.wav"`.
-}
type alias Handler effectA a effectB =
    effectA -> a -> Effects a effectB


{-| Initialize an `Effects` wrapper from a value and a list of effects.
-}
init : a -> List effect -> Effects a effect
init x effects =
    ( x, effects )


{-| Take a value and wrap it with no `Effects`.
This is useful when you don't want to surround your value in parentheses because
it will look messy, e.g. with updates to larger data structures.
-}
return : a -> Effects a effect
return x =
    init x []


{-| Gets the value from a value-and-effects pair.
-}
getValue : Effects a effect -> a
getValue ( x, effects ) =
    x


{-| Gets the effects from a value-and-effects pair.
-}
getEffects : Effects a effect -> List effect
getEffects ( x, effects ) =
    effects


{-| Add some additional side-effects to your value-and-effects result.
-}
add : List effect -> Effects a effect -> Effects a effect
add newEffects ( x, effects ) =
    init x (effects ++ newEffects)


{-| Same as `add`, but will only add the effects if the given condition is
satisfied. For example, you might only want to increase the score of the game
if the enemy died:

    Effects.return updatedEnemy
        |> Effects.addIf updatedEnemy.isDead [IncreaseScore 100]
-}
addIf : Bool -> List effect -> Effects a effect -> Effects a effect
addIf cond newEffects result =
    if cond then
        add newEffects result
    else
        result


{-| Modify the value of a value-and-effects pair, while still keeping the effects
   the same.
-}
mapOverValue : (a -> b) -> Effects a effect -> Effects b effect
mapOverValue fn ( x, effects ) =
    ( fn x, effects )


{-| Modify the effects of a value-and-effects pair, while still keeping the value
the same.
-}
mapOverEffects : (effectA -> effectB) -> Effects a effectA -> Effects a effectB
mapOverEffects fn ( x, effects ) =
    ( x, List.map fn effects )


{-| Perform a list of effects on an object, returning the new state of that
object and any side-effects that resulted from those effects occurring.
-}
handle : Handler effectA a effectB -> List effectA -> a -> Effects a effectB
handle effectHandlerFn effects x =
    List.foldl (\effect result -> result `andThen` effectHandlerFn effect)
        (return x)
        effects


{-| If an object returned no side-effect, this function allows you to ignore it
in a way that will fail to compile if the object's code is modified to return
side-effects in the future.
-}
ignoreUnused : None -> a -> Effects a effect
ignoreUnused _ x =
    return x


{-| If you have a bunch of operations that take a value and return a value-and-effects
pair, e.g. a value and a list of `Cmd`s to batch together, chaining them together
can be really annoying, because you constantly have to destructure the result
and grab the value, bringing it into the next statement.
`andThen` lets you take the value out of a value-and-effects pair, run a function
on it that returns a new value-and-effects pair, and batches the old effects together
with the new ones. For example, here are a bunch of actions chained together that
might generate sound effects and particles:

    Effects.return newEnemy
        `andThen` Enemy.move {x = 0, y = 1}
        `andThen` Enemy.makeYelpingNoiseOccasionally
-}
andThen : Effects a effect -> (a -> Effects b effect) -> Effects b effect
andThen ( x, effects ) fn =
    let
        ( y, newEffects ) =
            fn x
    in
        init y (effects ++ newEffects)


{-| Takes a list of results with the same type of effects and brings their
effects together into one list. This is useful when you map a function that
returns a value and effects over a list of objects, e.g.

    List.map (Enemy.update Enemy.TakeDamage) enemies
-}
batch : List (Effects a effect) -> Effects (List a) effect
batch results =
    let
        ( xs, effectsLists ) =
            List.unzip results
    in
        ( xs, List.concat effectsLists )


{-| Takes a pair of a value and a list of commands,
and turns it into a pair of a value and a single command by batching the commands
together.
-}
toCmd : Effects a (Cmd msg) -> ( a, Cmd msg )
toCmd ( x, cmds ) =
    x ! cmds
