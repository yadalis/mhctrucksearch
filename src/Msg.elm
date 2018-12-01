module Msg exposing (..)

import Model exposing (..)
import RemoteData exposing (..)

type Msg
    = OnFetchTrucks (WebData (List Truck))
        -- | Refresh Int
        -- | Roll