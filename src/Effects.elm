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
Elm events such as I/O and randomness - they can be internal app events, such as a message indicating that
the score of your game should increase or that you want to navigate back to the home page.

For example, having the player jump might update the state of the player and have a
side-effect of playing a sound and spawning some dust particles if the player
jumped off the ground. In that case, your player component might look like this:

    type Msg
      = Jump

    type Effect
      = SpawnDustParticles
      | PlaySound String

    update : Msg -> Model -> Effects Model Effect
    update msg model =
        case msg of
            Jump ->
                Effects.return { model | velocity = jumpVelocity }
                    |> Effects.add [PlaySound "jumpSound.wav"]
                    |> Effects.addIf model.isGrounded [SpawnDustParticles]

# Aliases
@docs Effects, None

# Constructors
@docs init, return

# Getters
@docs getValue, getEffects

# Adding Effects
@docs add, addIf

# Mapping
@docs mapOverValue, mapOverEffects

# Handling Effects
@docs Handler, handle, ignoreUnused

# Chaining Operations
@docs andThen

# Combining
@docs batch

# Conversion
@docs toCmd
-}

-- ALIASES


{-| `Effects a effect` combines a value of type `a` and a list of effects of
type `effect` into a single data structure.
-}
type alias Effects a effect =
    ( a, List effect )


{-| `None` is useful when updating a component shouldn't have any side effects on
the world. This way, you can still keep the structure of your component the same,
making your code consistent and flexible if you want to introduce side-effects later
on. For example, a cloud in the background might not have any side-effects on the world:

    type Msg = Tick Time

    type alias Effect = Effects.None

    update : Msg -> Model -> Effects Model Effect
    update msg model =
        -- implementation details
-}
type alias None =
    ()



-- CONSTRUCTORS


{-| Initialize an `Effects` wrapper from a value and a list of effects. For now,
this is pretty useless compared to just wrapping the value and effects
in parentheses -- however, later on the implementation of `Effects`
might change to become a dictionary with `value` and `effects` keys, so using
this function makes your code more future-proof. It's also a bit nicer stylistically
compared to wrapping your code in parentheses when the value is more complicated: compare
`elm-format`'s output of

    ( { x = 0
      , y = 0
      }
    , [ PlaySound "hello.wav", IncreaseScore 100 ]
    )

with

    Effects.init
        { x = 0
        , y = 0
        }
        [ PlaySound "hello.wav", IncreaseScore 100 ]

-}
init : a -> List effect -> Effects a effect
init x effects =
    ( x, effects )


{-| Take a value and wrap it with no effects.
This is useful when you don't want to make a tuple with a more complicated value and an empty
list because it will look messy, e.g.

    Effects.return
        { model
            | isGrounded = true
            , velocity = (0, 0)
        }
-}
return : a -> Effects a effect
return x =
    init x []



-- GETTERS


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



-- ADDING EFFECTS


{-| Add some additional side-effects to your value-and-effects result. This is
nice when you want to take the result from another function and dump some extra
effects into it without destructuring.
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



-- MAPPING


{-| Modify the value of a value-and-effects pair, while still keeping the effects
   the same.
-}
mapOverValue : (a -> b) -> Effects a effect -> Effects b effect
mapOverValue fn ( x, effects ) =
    ( fn x, effects )


{-| Map a function over the list of effects of a value-and-effects pair,
while still keeping the value the same.
-}
mapOverEffects : (effectA -> effectB) -> Effects a effectA -> Effects a effectB
mapOverEffects fn ( x, effects ) =
    ( x, List.map fn effects )



-- HANDLING EFFECTS


{-| A `Handler` is a function that takes an effect and applies it to an object,
giving back the new state of that object and any resulting effects.
-}
type alias Handler effectA a effectB =
    effectA -> a -> Effects a effectB


{-| Perform a list of effects sequentially on an object, returning the new state of that
object and any side-effects that resulted from those effects occurring. This is similar
to a `foldl`.

This is useful when you want to take a child's side-effect and have the parent react to it.
For example, if a child component gets updated and returns an effect saying to increase the score,
the parent should update the score and play a sound:

    -- in the parent component's update function

    let
        (updatedEnemies, enemyEffects) =
            List.map (Enemy.update Enemy.TakeDamage) model.enemies
                |> Effects.batch -- the score might increase if the enemy died
    in
        Effects.return { model | enemies = updatedEnemies }
            `Effects.andThen` Effects.handle handleEnemyEffect enemyEffects

    -- in the parent component's helper functions

    handleEnemyEffect : Effects.Handler Enemy.Effect Model Effect
    handleEnemyEffect enemyEffect model =
        case enemyEffect of
            Enemy.IncreaseScore amount ->
                Effects.return { model | score = model.score + amount }
                    |> Effects.add [PlaySound "scoreIncreased.wav"]
-}
handle : Handler effectA a effectB -> List effectA -> a -> Effects a effectB
handle effectHandlerFn effects x =
    List.foldl (\effect result -> result `andThen` effectHandlerFn effect)
        (return x)
        effects


{-| If an object returned no side-effect, this function allows you to ignore it
in a way that will fail to compile if the object's code is modified to return
side-effects in the future.

    Effects.return { model | clouds = updatedClouds }
        `Effects.andThen` Effects.handle handleCloudEffect cloudEffects

    handleCloudEffect : Effects.Handler Cloud.Effect Model Effect
    handleCloudEffect =
      Effects.ignoreUnused
-}
ignoreUnused : Handler None a effect
ignoreUnused _ x =
    return x



-- CHAINING


{-| If you have a bunch of operations that take a value and return a value-and-effects
pair, e.g. a value and a list of `Cmd`s to batch together, chaining them together
can be really annoying, because you constantly have to destructure the result
and grab the value, bringing it into the next statement.

`andThen` lets you take the value out of a value-and-effects pair, run a function
on it that returns a new value-and-effects pair, and batches the old effects together
with the new ones. For example, here are a bunch of actions chained together that
might generate sound effects and particles:

    -- returns Effects Enemy.Model Enemy.Effect
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



-- COMBINING


{-| Takes a list of results with the same type of effects and brings their
effects together into one list. This is useful when you map a function that
returns a value and effects over a list of objects, e.g.

    List.map (Enemy.update Enemy.TakeDamage) enemies
        |> Effects.batch -- equivalent to |> \(x, effectLists) -> (x, List.concat effectsLists)
-}
batch : List (Effects a effect) -> Effects (List a) effect
batch results =
    let
        ( xs, effectsLists ) =
            List.unzip results
    in
        ( xs, List.concat effectsLists )



-- CONVERSION


{-| Takes a pair of a value and a list of commands,
and turns it into a pair of a value and a single command by batching the commands
together.

    runRequests : Model -> (Model, Cmd Msg)
    runRequests model =
        (Effects.return model
            `Effects.andThen` fetchGifs
            `Effects.andThen` attachRandomId
        )
            |> Effects.toCmd
-}
toCmd : Effects a (Cmd msg) -> ( a, Cmd msg )
toCmd ( x, cmds ) =
    x ! cmds
