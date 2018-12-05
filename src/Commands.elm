module Commands exposing (..)

import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Msg exposing (..)
import Model  exposing (..)
--import RemoteData  exposing (..)
import Array exposing(..)

fetchTrucks: Cmd Msg
fetchTrucks =
    Http.get
        { url = fetchTrucksUrl
        --, expect = expectJson (RemoteData.fromResult >> OnFetchTrucks) fetchTrucksDecoder
        , expect = expectJson OnFetchTrucks fetchTrucksDecoder
        }

fetchTrucksUrl: String
fetchTrucksUrl =
        "http://localhost:13627/api/repairorder/gettrucks"

fetchTrucksDecoder: Decode.Decoder (List Truck)
fetchTrucksDecoder = 
    Decode.list trucksDecoder
          
trucksDecoder :  Decode.Decoder Truck
trucksDecoder  =
    Decode.succeed Truck  
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "stockNumber" Decode.int
        |> required "appraisalNumber" Decode.int
        |> required "poNumber" Decode.string
        |> required "price" Decode.int
        |> required "title" Decode.string
        |> required "condition" Decode.string
        |> required "make" Decode.string
        |> required "model" Decode.string
        |> required "apu" Decode.string
        |> required "cdl" Decode.string
        |> required "year" Decode.string
        --|> hardcoded "2000"
        |> required "primaryImageLink" Decode.string
        |> required "truckType" Decode.string
        |> required "salesStatus" Decode.string
        |> required "sleeperRoof" Decode.string
        |> required "sleeperBunk" Decode.string
    