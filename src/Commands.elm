module Commands exposing (..)

import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Msg exposing (..)
import Model  exposing (..)
import RemoteData  exposing (..)


fetchTrucks: Cmd Msg
fetchTrucks =
    Http.get
        { url = fetchTrucksUrl
        , expect = expectJson (RemoteData.fromResult >> OnFetchTrucks) fetchTrucksDecoder
        }

fetchTrucksUrl: String
fetchTrucksUrl =
        "http://localhost:13627/api/repairorder/gettruckfields"

fetchTrucksDecoder: Decode.Decoder (List Truck)
fetchTrucksDecoder = 
    Decode.list trucksDecoder
          
trucksDecoder :  Decode.Decoder Truck
trucksDecoder  =
    Decode.succeed Truck  
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "fields" (Decode.list itemFieldDecoder)

itemFieldDecoder :  Decode.Decoder ItemField
itemFieldDecoder  =
    Decode.succeed ItemField  
        |> required "value" Decode.string
        |> required "displayName" Decode.string