module Model exposing (..)

import RemoteData exposing (WebData)

type alias Truck =
    { 
          id : String
        , name : String
        , fields : List ItemField
    }

type alias ItemField =
    { 
          value : String
        , displayName : String
    }
    

type alias Model =
    { 
        trucks : WebData ( List Truck )
    }

initialModel : Model
initialModel =
    { 
        trucks = RemoteData.Loading
    }