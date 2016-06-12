module Effects exposing (Effects, Handler, None, init, return, getValue, getEffects, add, addIf, handle, ignoreUnused, andThen, batch, toCmd)


type alias Effects a effect =
    ( a, List effect )


type alias None =
    ()


type alias Handler effectA a effectB =
    effectA -> a -> Effects a effectB


init : a -> List effect -> Effects a effect
init x effects =
    ( x, effects )


return : a -> Effects a effect
return x =
    init x []


getValue : Effects a effect -> a
getValue ( x, effects ) =
    x


getEffects : Effects a effect -> List effect
getEffects ( x, effects ) =
    effects


add : List effect -> Effects a effect -> Effects a effect
add newEffects ( x, effects ) =
    init x (effects ++ newEffects)


addIf : Bool -> List effect -> Effects a effect -> Effects a effect
addIf cond newEffects result =
    if cond then
        add newEffects result
    else
        result


mapOverValue : (a -> b) -> Effects a effect -> Effects b effect
mapOverValue fn ( x, effects ) =
    ( fn x, effects )


mapOverEffects : (effectA -> effectB) -> Effects a effectA -> Effects a effectB
mapOverEffects fn ( x, effects ) =
    ( x, List.map fn effects )


handle : Handler effectA a effectB -> List effectA -> a -> Effects a effectB
handle effectHandlerFn effects x =
    List.foldl (\effect result -> result `andThen` effectHandlerFn effect)
        (return x)
        effects


ignoreUnused : None -> a -> Effects a effect
ignoreUnused _ x =
    return x


andThen : Effects a effect -> (a -> Effects b effect) -> Effects b effect
andThen ( x, effects ) fn =
    let
        ( y, newEffects ) =
            fn x
    in
        init y (effects ++ newEffects)


batch : List (Effects a effect) -> Effects (List a) effect
batch results =
    let
        ( xs, effectsLists ) =
            List.unzip results
    in
        ( xs, List.concat effectsLists )


toCmd : Effects a (Cmd msg) -> ( a, Cmd msg )
toCmd ( x, cmds ) =
    x ! cmds
