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
import List.Extra exposing (..)

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
        , expect = expectJson OnFetchTrucks fetchTrucksDecoder
        }

fetchAppraisedTrucks : String -> Cmd Msg
fetchAppraisedTrucks searchText =
    Http.get
        { url = fetchAppraisedTrucksUrl searchText
        , expect = expectJson OnFetchTrucks fetchTrucksDecoder
        }

fetchSearchFilterRanges: Cmd Msg
fetchSearchFilterRanges =
    Http.get
        { url = fetchSearchFilterRangesUrl
        , expect = expectJson OnFetchSearchFilterRanges onFetchSearchFilterRangesDecoder
        }

fetchTrucksUrl : String -> String -> String
fetchTrucksUrl truckCondition searchText =
        "http://127.0.0.1:3004/trks"
       

fetchAppraisedTrucksUrl : String -> String
fetchAppraisedTrucksUrl searchText =
        "http://127.0.0.1:3004/trks"

fetchSearchFilterRangesUrl: String
fetchSearchFilterRangesUrl =
        "http://127.0.0.1:3005/srchRanges"
       
        
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
        --|> required "truckType" Decode.string
        |> required "truckType" truckTypeDecoder
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
        |> required "brakeType" Decode.string
        |> required "rearAxleRatio" Decode.float
        |> required "exhaustType" Decode.string
        |> required "rearWheelSize" Decode.float
        |> required "frontWheelSize" Decode.float

truckTypeDecoder : Decode.Decoder String
truckTypeDecoder =
  Decode.string |> Decode.andThen 
    (
        \truckTypeValue ->
            case truckTypeValue of
            "I"   -> Decode.succeed   "Inventory"
            "A"   -> Decode.succeed   "Appraisal"
            _     -> Decode.succeed   "Purchase Order"
    )

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

searchFilterRangeUnionTypeDecoder : Decode.Decoder SearchFilterCustomType
searchFilterRangeUnionTypeDecoder = 
    Decode.string |> Decode.andThen searchFilterRangeUnionTypeString

searchFilterRangeUnionTypeString : String -> Decode.Decoder SearchFilterCustomType
searchFilterRangeUnionTypeString str =
    Decode.succeed <| convertStringToRangeSearchFilter str

convertStringToRangeSearchFilter rangeFilterStr =
    
    find (\sfRangeMeta -> sfRangeMeta.filterNameString == rangeFilterStr) rangeSearchFiltersInitialExpandState
        |> Maybe.map (\sfRangeMeta -> sfRangeMeta.filterName)
        -- the below condition should never happen unless you misspell in metadata list in model.elm file
        |> Maybe.withDefault Price