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
import BusinessFunctions.SearchFilterFunctions exposing (..)

import Json.Decode.Extra exposing (fromResult)

fetchTrucks searchFilterParam searchText pageNumber sortField sortOrder  =
    let
            url  = crossOrigin "http://localhost:50977/api/mhc/gettruckspaged" 
                                []
                                [
                                    string "filterString" searchFilterParam,
                                    string "searchText" searchText,
                                    int "pageNumber" pageNumber,
                                    string "sortField" sortField, string "sortOrder" sortOrder 
                                ]
            
            -- url = crossOrigin  "https://rest.mhc.com/trucks_rest/api/mhc/gettruckspaged" 
            --                     []
            --                     [
            --                         string "filterString" searchFilterParam,
            --                         string "searchText" searchText,
            --                         int "pageNumber" pageNumber,
            --                         string "sortField" sortField, string "sortOrder" sortOrder 
            --                     ]

            --asdf = Debug.log "url " [url]
    in
    
    Http.get
        { 
            
            url = url
        --, expect = expectJson (RemoteData.fromResult >> OnFetchTrucks) fetchTrucksDecoder
            , expect = expectJson OnFetchTrucks fetchTrucksDecoder
        }
 
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

fetchTrucksDecoder : Decode.Decoder TruckData
fetchTrucksDecoder = 
     Decode.succeed TruckData  
        |> required "pages" Decode.int
        |> required "searchFilters" (Decode.list searchFilterDecoder)
        |> required "finalFilteredTrucks" (Decode.list trucksDecoder)
        |> required "totalTrucksCount" Decode.int
        |> required "cleanSearchFilterBullets" Decode.bool
    
searchFilterDecoder :  Decode.Decoder SearchFilterType
searchFilterDecoder  =       
    Decode.succeed SearchFilterType  
        |> required "index" Decode.int
        |> required "searchFilterKey" Decode.string -- if you omit this, it returns partial func waiting to accept searchFilterKey
        |> required "searchFilterExtraData" Decode.string -- if you omit this, it returns partial func waiting to accept searchFilterKey
        |> required "userAction" stringBoolDecoder --Decode.bool
        |> required "resultCount" Decode.int
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

convertStringToRangeSearchFilter filterStr =
    find (\(filterNameString, filterName) -> filterNameString == filterStr)
            (List.map (\rf -> (rf.filterNameString, rf.filterName)) partialSearchFiltersMetadata)
    |> Maybe.map (\(filterNameString, filterName) -> filterName)
    -- the below condition should never happen unless you misspell in metadata list in model.elm file
    |> Maybe.withDefault Price