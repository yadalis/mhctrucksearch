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
        ,pagedTruckList : List Truck
        ,currentPageNumber : Int
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
        ,expandCollapseSearchFilterStates : Array SearchFilterState
        ,expandCollapseAllChecked : Bool
        
    }

type alias SearchFilterState =
    {
        index : Int
        ,searchFilterCustomType : SearchFilterCustomType
        ,userAction : Bool
    }
    
type SearchFilterCustomType
    = SalesStatus
    | Year
    | Make
    | MakeModel
    | SleeperRoof
    | SleeperBunk

type alias SearchFilterType =
    {   
        index : Int
        ,searchFilterKey : String
        ,userAction : Bool
        ,resultCount : Int
        ,filterCategory : SearchFilterCustomType
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
        ,pagedTruckList = []
        ,currentPageNumber = 1
    }

initalUIModel : String -> UIModel
initalUIModel jsFlag =
    {
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
                                            -- this is to initialize an Array, repeat creates one item in this case and that lets us push rest of the items
                                            -- this list can be generated off of datasource, when that happens we dont need to hardcode index value, just use indexedMap
                                            -- and set the generated index value to index prop
        expandCollapseSearchFilterStates = Array.repeat 1 {index = 0,searchFilterCustomType = SalesStatus, userAction = False} 
                                                |> Array.push {index = 1,searchFilterCustomType = Year, userAction = False}
                                                |> Array.push {index = 2,searchFilterCustomType = Make, userAction = True}
                                                |> Array.push {index = 3,searchFilterCustomType = MakeModel, userAction = False}
                                                |> Array.push {index = 4,searchFilterCustomType = SleeperRoof, userAction = False}
                                                |> Array.push {index = 5,searchFilterCustomType = SleeperBunk, userAction = False},
        expandCollapseAllChecked = False
    }