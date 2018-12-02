module Helpers.Utils exposing (..)

import Html.Events exposing (on, targetValue)
import Json.Decode as Decode

onChange  tagger =
    on "change" (Decode.map tagger targetValue)

appendIf flag value list =
    if flag == True then
        --list ++ [value] -- this is fine as well, to add the item to the list
        value :: list
    else
        list

buildQueryString list =
    list    
        |> List.map (\(a,b) -> a ++ "=" ++ b)
        |> String.join "&"
        |> (++) "?"

apiURL str =
    "https://opentdb.com/api.php" ++ str