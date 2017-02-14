module Utils
    exposing
        ( (=>)
        , swap
        , indexedPair
        , indexedPair2
        , list
        , listFind
        , listContains
        , applyWhen
        , applyUnless
        , on
        , liftMaybeToTuple
        , isNothing
        )

{-| Additional basic functions.
# Tuples
@docs (=>), swap
-}


{-| A shorthand for writing 2-tuples. Very commonly used when expressing key/value pairs
in CSS or Json encoders.
-}
(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| Swaps the elements in a pair.
    swap ( 1, 2 ) == ( 2, 1 )
-}
swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


indexedPair : (a -> b) -> a -> ( b, a )
indexedPair f a =
    ( f a, a )


indexedPair2 : (a -> b) -> a -> ( a, b )
indexedPair2 f =
    swap << indexedPair f


list : a -> List a
list a =
    [ a ]


applyWhen : Bool -> (a -> a) -> a -> a
applyWhen b f thing =
    if b then
        f thing
    else
        thing


applyUnless : Bool -> (a -> a) -> a -> a
applyUnless b =
    applyWhen (not b)


listFind : (a -> Bool) -> List a -> Maybe a
listFind f list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if f first then
                Just first
            else
                listFind f rest


listContains : a -> List a -> Bool
listContains x list =
    case listFind ((==) x) list of
        Just _ ->
            True

        Nothing ->
            False


liftMaybeToTuple : ( a, Maybe b ) -> Maybe ( a, b )
liftMaybeToTuple ( a, m ) =
    case m of
        Just b ->
            Just ( a, b )

        Nothing ->
            Nothing


isNothing : Maybe a -> Bool
isNothing a =
    case a of
        Nothing ->
            True

        _ ->
            False


on : (a -> b -> c) -> (x -> a) -> (x -> b) -> x -> c
on f ga gb x =
    f (ga x) (gb x)
