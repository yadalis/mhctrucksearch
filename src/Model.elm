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
        , suspension        : String
        , bodyType          : String
        , sleeperBunk       : String
        , salesStatus       : String
        , sleeperRoof       : String
        , rearAxleType      : String
        , sleeperInches     : Float
        , wheelBase         : Float
        , chassisNumber     : String
        , transType         : String
        , mileage           : Float
        , frontAxleWeight   : Float        
        , rearAxleWeight    : Float        
        , fleetCode         : String
        , specialFinancing  : String
        , inventoryAge      : Float
        , owningBranch      : String
        , location          : String
        , locationName      : String
        , salesStatusFlag   : String
        , hasPhoto          : String
        , brakeType         : String
        , rearAxleRatio     : Float
        , exhaustType       : String
        , rearWheelSize     : Float
        , frontWheelSize    : Float
    }

type alias Model =
    { 
        -- trucks : WebData ( Array Truck )
        -- ,
        truckList : List Truck
        ,totalTrucksCount : Int
        ,filteredTruckList : List Truck
        ,pagedTruckList : List Truck

    }

type alias UIModel =
    {   
        searchString : String
        ,onLoadSearchFilters : List String -- to support value from javascript on initial app load
        ,allSearchFilters : List SearchFilterType
        ,selectedFilterBullets : List SearchFilterType
        ,expandCollapseSearchFilterStates : Array SearchFilterState
        ,showDropdown : Bool
        ,showLoader : Bool
        ,currentSortByMetaData : SortMetaData
        ,currentSortOrder : SortOrder
        ,hasErrorsToPresent : Bool
        ,hasWarningsToPresent : Bool
        ,userWarningMessage : String
        ,initialLoad : Bool
        ,currentPageNumber : Int
        ,totalPages : Int
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
    | EngineMake
    | TransType
    | Suspension
    | BodyType
    | RearAxleType
    | TruckType
    | FleetCode
    | SpecialFinancing
    | OwningBranch
    | APU
    | CDL
    | Photo
    | BrakeType
    | ExhaustType 
    -- range filters
    | Price
    | EngineHP
    | SleeperInches
    | WheelBase
    | Mileage
    | FrontAxleWeight
    | RearAxleWeight
    | InventoryAge
    | LocationName
    | RearAxleRatio
    | RearWheelSize
    | FrontWheelSize

type SortOrder
    = SortASC
    | SortDESC

type SortBy
    = PriceLowToHigh
    | PriceHighToLow
    | MileageLowToHigh
    | MileageHighToLow
    | MakeAtoZ
    | MakeZtoA
    | YearNewToOld
    | YearOldToNew

type alias SortMetaData =
    {
        --sortKey : String,
        sortItemDisplayText : String,
        sortBy : String,
        sortByField : SortBy,
        sortOrder : String
    }

type alias TruckData =
    {   
        pages : Int
        ,searchFilters : List SearchFilterType
        ,trucks : List Truck
        ,totalTrucksCount : Int
        ,cleanSearchFilterBullets : Bool
    }


type alias SearchFilterType =
    {   
        index : Int
        ,searchFilterKey : String
        ,searchFilterExtraData : String
        ,userAction : Bool
        ,resultCount : Int
        ,filterCategory : SearchFilterCustomType
    }

type alias SelectedSearchFilterBulletType =
    {   
        index : Int
        ,searchFilterKey : String
        ,searchFilterExtraData : String
        ,userAction : Bool
        ,resultCount : Int
        ,filterCategory : SearchFilterCustomType
    }

defaultSearchFiltersMetadata =  
                            {
                                    filterName = FleetCode,
                                    displayText = "Default",
                                    filters = Array.empty,
                                    expandByDefault = False, 
                                    truckFieldFunction = (List.map .fleetCode, \sf t -> String.trim t.fleetCode == sf),
                                    truckRangeFieldFunction = (\minValue maxValue t -> t.price >= minValue && t.price <= maxValue),
                                    filterNameString = "Price"
                            }

initialModel : Model
initialModel =
    { 
        -- trucks = RemoteData.Loading
        -- ,
        truckList = [] -- Array.empty
        ,totalTrucksCount = 0
        ,filteredTruckList = []
        ,pagedTruckList = []

    }
 

searchFiltersInitialExpandState = 
    [
          {filterName = FleetCode,        expandByDefault = False}
        , {filterName = SalesStatus,      expandByDefault = True}
        , {filterName = TruckType,        expandByDefault = False}
        , {filterName = SpecialFinancing, expandByDefault = False}
        , {filterName = Year,             expandByDefault = False}
        , {filterName = Make,             expandByDefault = True}
        , {filterName = MakeModel,        expandByDefault = False}         
        , {filterName = SleeperRoof,      expandByDefault = False}
        , {filterName = SleeperBunk,      expandByDefault = False}
        , {filterName = EngineMake,       expandByDefault = False}
        , {filterName = TransType,        expandByDefault = False}
        , {filterName = Suspension,       expandByDefault = False}
        , {filterName = RearAxleType,     expandByDefault = False}
        , {filterName = LocationName,     expandByDefault = False}
        , {filterName = OwningBranch,     expandByDefault = False}
        , {filterName = BodyType,         expandByDefault = False}
        , {filterName = APU,              expandByDefault = False}
        , {filterName = CDL,              expandByDefault = False}
        , {filterName = Photo,            expandByDefault = False}
        , {filterName = BrakeType,        expandByDefault = False}
        , {filterName = ExhaustType,      expandByDefault = False}
        , {filterName = Price,            expandByDefault = True}
        , {filterName = SleeperInches,    expandByDefault = False}
        , {filterName = EngineHP,         expandByDefault = False}
        , {filterName = WheelBase,        expandByDefault = False}
        , {filterName = FrontAxleWeight,  expandByDefault = False}
        , {filterName = RearAxleWeight,   expandByDefault = False}
        , {filterName = InventoryAge,     expandByDefault = False}
        , {filterName = Mileage,          expandByDefault = False}
        , {filterName = RearAxleRatio,    expandByDefault = False}
        , {filterName = RearWheelSize,    expandByDefault = False}
        , {filterName = FrontWheelSize,   expandByDefault = False}

    ]

initalUIModel : String -> UIModel
initalUIModel jsFlag =
    {
        searchString = "",
        onLoadSearchFilters  = String.split "&" jsFlag,
        -- 
        allSearchFilters = [],
        selectedFilterBullets = [],
        expandCollapseSearchFilterStates = 
                Array.fromList <|
                             (List.indexedMap 
                                        (
                                            \index searchFilterTypeRecord ->
                                                        {index = index,searchFilterCustomType = searchFilterTypeRecord.filterName, userAction = searchFilterTypeRecord.expandByDefault}
                                        )
                            searchFiltersInitialExpandState)
                         ,
        showDropdown = False,
        showLoader = False,
        currentSortByMetaData =  {sortItemDisplayText = "Make A to Z", sortBy = "Make", sortByField = MakeAtoZ, sortOrder = "ASC"},
        currentSortOrder = SortDESC,
        hasErrorsToPresent = False,
        hasWarningsToPresent = False,
        initialLoad = True,
        userWarningMessage = "",
        currentPageNumber = 1,
        totalPages = 1
    }