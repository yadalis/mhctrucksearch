module Msg exposing (..)

import Model exposing (..)
--import RemoteData exposing (..)
import Array exposing (..)
import Http exposing(..)
import Json.Decode as Decode
import Json.Encode as Encode

type Msg
    --= OnFetchTrucks (WebData (Array Truck))
    =
        --OnFetchTrucks (Result Error (List Truck) )
        OnFetchTrucks (Result Error TruckData )
    --| OnFetchSearchFilterRanges (Result Error (List SearchFilterType) )
    --| OnFetchSearchFilters (Result Error (List SearchFilterType) )
    --| OnFetchSearchFilters (Result Error SearchFiltersAndPages )
    --| OnFetchSearchFilterRanges (Result Error (List SearchFilterRangeType) )
    --| FilterCheckBoxClicked Int SearchFilterCustomType String String Bool  -- might want to use ModelSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    | FilterCheckBoxClicked SearchFilterType Bool  -- might want to use ModelSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    --| FilterRangeCheckBoxClicked Int SearchFilterRangeUnionType Bool -- might want to use ModelSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    | SearchString String
    --| ClearSearchStringResults
    | SearchPressed
    | HandleKeyboardEvent
    | CollapseClicked SearchFilterState Bool
--    | CollapseRangeClicked SearchFilterRangeState Bool
    | CollapseAllClicked Bool
    | PageNumberClicked Int
    | OperateSortDialog Bool
    | CloseUserWarningsDialog Bool
    | SortTrucks SortBy  SortOrder
    --| ApplyFilters
    --| ShowAppraisedTrucks Bool
    -- | WorkWithAppraisedTrucks  Bool
    -- | WorkWithNewTrucks Bool
    | ShowLoader Bool
    --| ShowTrucksWithPhotoOnly
    | ClearAllFilters
