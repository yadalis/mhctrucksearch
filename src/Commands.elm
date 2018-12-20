module Commands exposing (..)

import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Msg exposing (..)
import Model  exposing (..)
--import RemoteData  exposing (..)
import Array exposing(..)
import Url.Builder exposing (..)

import Json.Decode.Extra exposing (fromResult)

fetchTrucks : String -> Cmd Msg
fetchTrucks searchText =
    Http.get
        { url = fetchTrucksUrl searchText
        --, expect = expectJson (RemoteData.fromResult >> OnFetchTrucks) fetchTrucksDecoder
        , expect = expectJson OnFetchTrucks fetchTrucksDecoder
        }

fetchSearchFilterRanges: Cmd Msg
fetchSearchFilterRanges =
    Http.get
        { url = fetchSearchFilterRangesUrl
        --, expect = expectJson (RemoteData.fromResult >> OnFetchTrucks) fetchTrucksDecoder
        , expect = expectJson OnFetchSearchFilterRanges onFetchSearchFilterRangesDecoder
        }

fetchTrucksUrl : String -> String
fetchTrucksUrl searchText =
        --"http://localhost:13627/api/repairorder/gettrucks"
        --http://172.21.123.180/NewMHCtruckSync/api/mhc/gettrucks
            if String.isEmpty searchText then
                "http://localhost:50977/api/mhc/gettrucks"
            else
                crossOrigin "http://localhost:50977/api/mhc/gettrucks" [searchText] []

            -- if String.isEmpty searchText then
            --     "http://172.21.123.180/NewMHCtruckSync/api/mhc/gettrucks"
            -- else
            --     crossOrigin "http://172.21.123.180/NewMHCtruckSync/api/mhc/gettrucks" [searchText] []
        --"http://localhost:3333/trks"


fetchSearchFilterRangesUrl: String
fetchSearchFilterRangesUrl =
        --"http://localhost:13627/api/repairorder/gettrucks"
        --"http://localhost:50977/api/repairorder/gettrucks"
        "http://localhost:4444/srchRanges"
        
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
        |> required "price" Decode.float
        |> required "title" Decode.string
        |> required "condition" Decode.string
        |> required "make" Decode.string
        |> required "model" Decode.string
        |> required "engineMake" Decode.string
        |> required "engineModel" Decode.string
        |> required "engineHP" Decode.float
        |> required "apu" Decode.string
        |> required "cdl" Decode.string
        |> required "year" Decode.string
        --|> hardcoded "2000"
        |> required "primaryImageLink" Decode.string
        |> required "truckType" Decode.string
        |> required "salesStatus" Decode.string
        |> required "sleeperRoof" Decode.string
        |> required "sleeperBunk" Decode.string
        |> required "sleeperInches" Decode.string
        |> required "chassisNumber" Decode.string
        |> required "transType" Decode.string
        |> required "mileage" Decode.float
        |> required "locationNumber" Decode.string
        |> required "locationName" Decode.string
        |> required "salesStatusFlag" Decode.string
    

onFetchSearchFilterRangesDecoder : Decode.Decoder (List SearchFilterType)
onFetchSearchFilterRangesDecoder = 
    Decode.list searchFilterRangeDecoder

      
searchFilterRangeDecoder :  Decode.Decoder SearchFilterType
searchFilterRangeDecoder  =       
    Decode.succeed SearchFilterType  
        |> hardcoded 0
        |> required "searchFilterKey" Decode.string -- if you omit this, it returns partial func waiting to accept searchFilterKey
        |> required "searchFilterExtraData" Decode.string -- if you omit this, it returns partial func waiting to accept searchFilterKey
        -- |> required "searchFilterMinValue" Decode.int
        -- |> required "searchFilterMaxValue" Decode.int
        |> required "userAction" stringBoolDecoder
        |> hardcoded 0
        |> required "filterCategory" searchFilterRangeUnionTypeDecoder

stringBoolDecoder : Decode.Decoder Bool
stringBoolDecoder =
  Decode.string |> Decode.andThen (\val ->
    case val of
      "True" -> Decode.succeed True
      "False" -> Decode.succeed False
      _ -> Decode.fail <| "Expecting \"true\" or \"false\" but found " ++ val )

-- the commented code is good as well
-- searchFilterCustomTypeDecoder : Decode.Decoder SearchFilterCustomType
-- searchFilterCustomTypeDecoder = 
--     Decode.string |> Decode.andThen (fromResult << parseSearchFilterRangeTypeString)

-- parseSearchFilterRangeTypeString : String -> Result String SearchFilterCustomType
-- parseSearchFilterRangeTypeString string =

--             case string of
--                 "Price" ->
--                     Ok Year
                        

--                 "RearAxleWeight" ->
--                     Ok Make
                        

--                 "FrontAxleWeight" ->
--                     Ok MakeModel
                            

--                 _ ->
--                     Ok Year

searchFilterRangeUnionTypeDecoder : Decode.Decoder SearchFilterCustomType
searchFilterRangeUnionTypeDecoder = 
    Decode.string |> Decode.andThen searchFilterRangeUnionTypeString

searchFilterRangeUnionTypeString : String -> Decode.Decoder SearchFilterCustomType
searchFilterRangeUnionTypeString string =
            case string of
                "Price" ->
                    Decode.succeed Price
                "EngineHP" ->
                    Decode.succeed EngineHP
                
                _ ->
                    Decode.succeed Price