module Events exposing (onSelect)

import Json.Decode as Decode
import Html.Events exposing (on, targetValue)
import Html


emptyIsNothing : String -> Maybe String
emptyIsNothing s =
    if s == "" then
        Nothing
    else
        Just s


maybeTargetValue : Decode.Decoder (Maybe String)
maybeTargetValue =
    Decode.map emptyIsNothing targetValue


{-| An event handler for `<select>` tags. Set the child `<option>` tag's value to "" to get a `Nothing`.
-}
onSelect : (Maybe String -> msg) -> Html.Attribute msg
onSelect f =
    on "change"
        (Decode.map f maybeTargetValue)
