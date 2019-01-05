module Commands exposing (..)

import Model exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Msg exposing (..)
import Model  exposing (..)
--import RemoteData  exposing (..)
import Array exposing(..)
import Url.Builder exposing (..)

import Json.Decode.Extra exposing (fromResult)


getFetchURL truckCondition srchString isWorkingWithAppraisedTrucks =
    if isWorkingWithAppraisedTrucks then
        fetchAppraisedTrucks srchString      
    else
        fetchTrucks truckCondition srchString

fetchTrucks : String -> String -> Cmd Msg
fetchTrucks truckCondition searchText =
    Http.get
        { url = fetchTrucksUrl truckCondition searchText
        --, expect = expectJson (RemoteData.fromResult >> OnFetchTrucks) fetchTrucksDecoder
        , expect = expectJson OnFetchTrucks fetchTrucksDecoder
        }

fetchAppraisedTrucks : String -> Cmd Msg
fetchAppraisedTrucks searchText =
    Http.get
        { url = fetchAppraisedTrucksUrl searchText
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

fetchTrucksUrl : String -> String -> String
fetchTrucksUrl truckCondition searchText =
        --"http://localhost:13627/api/repairorder/gettrucks"
        --http://172.21.123.180/NewMHCtruckSync/api/mhc/gettrucks
            if String.isEmpty searchText then
                crossOrigin "http://localhost:50977/api/mhc/gettrucks" [truckCondition] []
            else
                crossOrigin "http://localhost:50977/api/mhc/gettrucks" [truckCondition, searchText] []

            -- if String.isEmpty searchText then
            --     crossOrigin "http://172.21.123.180/NewMHCtruckSyncAPILive/api/mhc/gettrucks"  [truckCondition] []
            -- else
            --     crossOrigin "http://172.21.123.180/NewMHCtruckSyncAPILive/api/mhc/gettrucks" [truckCondition, searchText] []
        -- "http://localhost:3333/trks"

fetchAppraisedTrucksUrl : String -> String
fetchAppraisedTrucksUrl searchText =
        --"http://localhost:13627/api/repairorder/gettrucks"
        --http://172.21.123.180/NewMHCtruckSync/api/mhc/gettrucks
            if String.isEmpty searchText then
                "http://localhost:50977/api/mhc/getappraisedtrucks"
            else
                crossOrigin "http://localhost:50977/api/mhc/getappraisedtrucks"  [searchText] []

            -- if String.isEmpty searchText then
            --     "http://172.21.123.180/NewMHCtruckSyncAPILive/api/mhc/getappraisedtrucks"
            -- else
            --     crossOrigin "http://172.21.123.180/NewMHCtruckSyncAPILive/api/mhc/getappraisedtrucks" [searchText] []
        -- "http://localhost:3333/trks"


fetchSearchFilterRangesUrl: String
fetchSearchFilterRangesUrl =
        --"http://localhost:13627/api/repairorder/gettrucks"
        --"http://localhost:50977/api/repairorder/gettrucks"
        --"http://localhost:4444/srchRanges"
        "http://localhost:50977/api/mhc/getrangefilters"
        --"http://172.21.123.180/NewMHCtruckSyncAPILive/api/mhc/getrangefilters"
        
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
        |> required "primaryImageLink" Decode.string
        |> required "truckType" Decode.string
        |> required "suspension" Decode.string
        |> required "bodyType" Decode.string
        |> required "sleeperBunk" Decode.string
        |> required "salesStatus" Decode.string
        |> required "sleeperRoof" Decode.string
        |> required "rearAxleType" Decode.string        
        |> required "sleeperInches" Decode.float
        |> required "wheelBase" Decode.float
        |> required "chassisNumber" Decode.string
        |> required "transType" Decode.string
        |> required "mileage" Decode.float
        |> required "frontAxleWeight" Decode.float
        |> required "rearAxleWeight" Decode.float
        |> required "fleetCode" Decode.string
        |> required "specialFinancing" Decode.string
        |> required "inventoryAge" Decode.float
        |> required "owningBranch" Decode.string
        |> required "locationNumber" Decode.string
        |> required "locationName" Decode.string
        |> required "salesStatusFlag" Decode.string
        |> required "hasPhoto" Decode.string
        
    

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
        |> required "userAction" stringBoolDecoder --Decode.bool
        |> hardcoded 0
        |> required "filterCategory" searchFilterRangeUnionTypeDecoder

stringBoolDecoder : Decode.Decoder Bool
stringBoolDecoder =
  Decode.string |> Decode.andThen (\val ->
    case String.toLower val of
      "true" -> Decode.succeed True
      "false" -> Decode.succeed False
      _ -> Decode.fail <| "Expecting \"true\" or \"false\" but found " ++ val )

-- the commented code is good as well
-- searchFilterCustomTypeDecoder : Decode.Decoder SearchFilterCustomType
-- searchFilterCustomTypeDecoder = 
--     Decode.string |> Decode.andThen (fromResult << parseSearchFilterRangeTypeString)

searchFilterRangeUnionTypeDecoder : Decode.Decoder SearchFilterCustomType
searchFilterRangeUnionTypeDecoder = 
    Decode.string |> Decode.andThen searchFilterRangeUnionTypeString

searchFilterRangeUnionTypeString : String -> Decode.Decoder SearchFilterCustomType
searchFilterRangeUnionTypeString str =
    Decode.succeed <| convertStringToRangeSearchFilter str

convertStringToRangeSearchFilter rangeFilterStr =
    allRangeFilterTypesKeyValueParis
        |> List.filter(\(k, v) -> k == rangeFilterStr)
        |> List.head
        |> Maybe.map (\(k, v) -> v)
        |> Maybe.withDefault Price --introduce noValue filter type to handle this situation