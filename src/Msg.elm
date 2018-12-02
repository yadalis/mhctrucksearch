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
    | FilterCDLNoCheckBoxClicked Bool
    | FilterCDLYesCheckBoxClicked Bool
    | FilterYearCheckBoxClicked Int Bool
    | SearchString String
    | SearchPressed