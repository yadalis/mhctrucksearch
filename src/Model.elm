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
--        , year              : Int
        , year              : String
        , primaryImageLink  : String
        , truckType         : String
        , salesStatus       : String
        , sleeperRoof       : String
        , sleeperBunk       : String
    }

type alias Model =
    { 
        -- trucks : WebData ( Array Truck )
        -- ,
        truckList : List Truck
        ,filteredTruckList : List Truck
    }

type alias UIModel =
    {   
        filterCDLNoSelected : Bool
        ,filterCDLYesSelected : Bool
        ,searchString : String
        ,onLoadSearchFilters : List String
        ,yearFilters : Array SearchFilterType
        ,makeFilters : Array SearchFilterType
        ,modelFilters : Array SearchFilterType
        ,salesStatusFilters : Array SearchFilterType
        ,sleeperRoofFilters : Array SearchFilterType
        ,sleeperBunkFilters : Array SearchFilterType
        ,expandCollapseSalesStatusChecked : Bool
        ,expandCollapseYearChecked : Bool
        ,expandCollapseAllChecked : Bool
        ,selectedFilterItems : Array SearchFilterType
    }

type alias SearchFilterType =
    {   
        index : Int
        ,searchFilterKey : String
        ,userAction : Bool
        ,resultCount : Int
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
        ,filteredTruckList = []
    }

initalUIModel : String -> UIModel
initalUIModel jsFlag =
    {
        --filterSelectionsModel = FilterSelectionsModel False False
        filterCDLNoSelected = False,
        filterCDLYesSelected = False,
        searchString = "",
        onLoadSearchFilters  = String.split "&" jsFlag,
        yearFilters = Array.empty,
        makeFilters = Array.empty,
        modelFilters = Array.empty,
        salesStatusFilters = Array.empty,
        sleeperRoofFilters = Array.empty,
        sleeperBunkFilters = Array.empty,
        expandCollapseSalesStatusChecked = False,
        expandCollapseYearChecked = False,
        expandCollapseAllChecked = True,
        selectedFilterItems = Array.empty
    }