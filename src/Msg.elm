module Msg exposing (..)

import Model exposing (..)
--import RemoteData exposing (..)
import Array exposing (..)
import Http exposing(..)
import Json.Decode as Decode
import Json.Encode as Encode

type Msg
    --= OnFetchTrucks (WebData (Array Truck))
    = OnFetchTrucks (Result Error (List Truck) )
    -- | FilterCDLNoCheckBoxClicked Bool
    -- | FilterCDLYesCheckBoxClicked Bool
    -- | FilterYearCheckBoxClicked Int Bool -- index year and userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    -- | FilterMakeCheckBoxClicked Int Bool-- might want to use MakeSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    -- | FilterModelCheckBoxClicked Int Bool -- might want to use ModelSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    -- | FilterSalesStatusCheckBoxClicked Int Bool -- might want to use ModelSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    -- | FilterSleeperRoofCheckBoxClicked Int Bool -- might want to use ModelSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    -- | FilterSleeperBunkCheckBoxClicked Int Bool -- might want to use ModelSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    | FilterCheckBoxClicked Int SearchFilterCustomType Bool -- might want to use ModelSearchFitler type instead of individual field types -- index make resultCount userAction - bool must be the last arg, since onclick events send user action to that last var automatically to the message
    | SearchString String
    | SearchPressed

type SearchFilterCustomType
    = SalesStatus
    | Year
    | Make
    | MakeModel
    | SleeperRoof
    | SleeperBunk