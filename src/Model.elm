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
        , price             : Float
        , title             : String
        , condition         : String
        , make              : String
        , model             : String
        , engineMake        : String
        , engineModel       : String
        , engineHP          : Float
        , apu               : String
        , cdl               : String
        , year              : String
        , primaryImageLink  : String
        , truckType         : String
        , salesStatus       : String
        , sleeperRoof       : String
        , sleeperBunk       : String
        , sleeperInches     : String
        , chassisNumber     : String
        , transType         : String
        , mileage           : Float
        , location          : String
        , locationName      : String
        , salesStatusFlag   : String
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
        ,priceFilters : Array SearchFilterType
        ,engineHPFilters : Array SearchFilterType
        --,priceFilters : Array SearchFilterRangeType
        ,expandCollapseSearchFilterStates : Array SearchFilterState
        --,expandCollapseSearchFilterRangeStates : Array SearchFilterRangeState
        ,expandCollapseAllChecked : Bool
        ,showDropdown : Bool
        ,currentSortBy : SortBy
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
    | Price
    | EngineHP

type SortOrder
    = SortASC
    | SortDSC

type SortBy
    = PriceLowToHigh
    | PriceHighToLow
    | MileageLowToHigh
    | MileageHighToLow
    | MakeAtoZ
    | MakeZtoA
    | YearNewToOld
    | YearOldToNew


type alias SearchFilterType =
    {   
        index : Int
        ,searchFilterKey : String
        ,searchFilterExtraData : String
        ,userAction : Bool
        ,resultCount : Int
        ,filterCategory : SearchFilterCustomType
    }

type alias FilterSelectionsModel =
    {   
        filterCDLNoSelected : Bool
        ,filterCDLYesSelected : Bool
    }

allFilterTypesMasterListWithItsInitialState = 
                        [ {filterName = SalesStatus, expandByDefault = False}
                        , {filterName = Year, expandByDefault = False}
                        , {filterName = Make, expandByDefault = True}
                        , {filterName = MakeModel, expandByDefault = False}
                        , {filterName = SleeperRoof, expandByDefault = True}
                        , {filterName = SleeperBunk, expandByDefault = True}
                        , {filterName = Price, expandByDefault = True}
                        , {filterName = EngineHP, expandByDefault = False}] 

allRangeFilterTypesMasterList = 
                        [ Price
                        , EngineHP]

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
        priceFilters = Array.empty,
        engineHPFilters = Array.empty,
        expandCollapseSearchFilterStates = 
                        Array.indexedMap 
                                        (
                                            \index searchFilterTypeRecord ->
                                                    {index = index,searchFilterCustomType = searchFilterTypeRecord.filterName, userAction = searchFilterTypeRecord.expandByDefault}
                                        )
                        <| Array.fromList allFilterTypesMasterListWithItsInitialState

        ,expandCollapseAllChecked = False,
        showDropdown = False,
        currentSortBy = MakeAtoZ
    }