module Helpers.Utils exposing (..)

import Html.Events exposing (on, targetValue)
import Json.Decode as Decode
import Task

defaultTrucksPerPage = 150

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


    -- this is just to send a message back in to update function when there are no side-effects needed and just a way to put the message back in to update function
sendMessage : msg -> Cmd msg
sendMessage msg =
    Task.succeed msg
        |> Task.perform identity -- dont know what is identity, find out.