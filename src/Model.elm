module Model exposing (..)

--import RemoteData exposing (WebData)
import Array exposing(..)

type alias Truck =
    { 
          id                : String
        , name              : String
        , stockNumber       : Int
        , appraisalNumber   : Int
        , poNumber          : String   
        , price             : Int
        , title             : String
        , condition         : String
        , make              : String
        , model             : String
        , apu               : String
        , cdl               : String
        , year              : Int
        , primaryImageLink  : String
        , truckType         : String
    }

type alias Model =
    { 
        -- trucks : WebData ( Array Truck )
        -- ,
        truckList : List Truck
        , textVal : String
    }

type alias UIModel =
    {   
        filterCDLNoSelected : Bool
        ,filterCDLYesSelected : Bool
        ,searchString : String
    }

type alias FilterSelectionsModel =
    {   
        filterCDLNoSelected : Bool
        ,filterCDLYesSelected : Bool
    }

initialModel : Model
initialModel =
    { 
        -- trucks = RemoteData.Loading
        -- ,
        truckList = [] -- Array.empty
        , textVal = "Init Model"
    }

initalUIModel : UIModel
initalUIModel =
    {
        --filterSelectionsModel = FilterSelectionsModel False False
        filterCDLNoSelected = False,
        filterCDLYesSelected = False,
        searchString = ""
    }