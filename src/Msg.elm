module Msg exposing (..)

import Model exposing (..)
--import RemoteData exposing (..)
import Array exposing (..)
import Http exposing(..)

type Msg
    --= OnFetchTrucks (WebData (Array Truck))
    = OnFetchTrucks (Result Error (List Truck) )
    | OnFetchSearchFilterRanges (Result Error (List SearchFilterType) )
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
    | SortTrucks SortBy
    --| ShowAppraisedTrucks Bool
    | WorkWithAppraisedTrucks  Bool
    | WorkWithNewTrucks Bool
    | ShowLoader Bool
    --| ShowTrucksWithPhotoOnly
    | ClearAllFilters
    | NOoP
