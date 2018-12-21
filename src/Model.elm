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
        , truckStatus       : String
        , specialFinancing  : String
        , inventoryAge      : Float
        , owningBranch      : String
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
        ,engineMakeFilters : Array SearchFilterType
        ,transTypeFilters : Array SearchFilterType
        ,suspensionFilters : Array SearchFilterType
        ,bodyTypeFilters : Array SearchFilterType
        ,rearAxleTypeFilters : Array SearchFilterType
        ,priceFilters : Array SearchFilterType
        ,engineHPFilters : Array SearchFilterType
        ,sleeperInchesFilters : Array SearchFilterType
        ,wheelBaseFilters : Array SearchFilterType
        ,mileageFilters : Array SearchFilterType
        ,frontAxleWeightFilters : Array SearchFilterType
        ,rearAxleWeightFilters : Array SearchFilterType
        ,fleetCodeFilters : Array SearchFilterType
        ,truckStatusFilters : Array SearchFilterType
        ,specialFinancingFilters : Array SearchFilterType
        ,inventoryAgeFilters : Array SearchFilterType        
        ,owningBranchFilters : Array SearchFilterType
        ,expandCollapseSearchFilterStates : Array SearchFilterState
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
    | EngineMake
    | TransType
    | Suspension
    | BodyType
    | RearAxleType
    | FleetCode
    | TruckStatus
    | SpecialFinancing
    | OwningBranch
    -- range filters
    | Price
    | EngineHP
    | SleeperInches
    | WheelBase
    | Mileage
    | FrontAxleWeight
    | RearAxleWeight
    | InventoryAge

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
                        , {filterName = Year, expandByDefault = True}
                        , {filterName = Make, expandByDefault = False}
                        , {filterName = MakeModel, expandByDefault = False}
                        , {filterName = SleeperRoof, expandByDefault = False}
                        , {filterName = SleeperBunk, expandByDefault = False}
                        , {filterName = EngineMake, expandByDefault = False}
                        , {filterName = TransType, expandByDefault = False}
                        , {filterName = Suspension, expandByDefault = False}
                        , {filterName = BodyType, expandByDefault = False}
                        , {filterName = RearAxleType, expandByDefault = False}
                        , {filterName = Price, expandByDefault = False}
                        , {filterName = EngineHP, expandByDefault = False}
                        , {filterName = SleeperInches, expandByDefault = False}
                        , {filterName = WheelBase, expandByDefault = False}
                        , {filterName = Mileage, expandByDefault = False}
                        , {filterName = FrontAxleWeight, expandByDefault = False}
                        , {filterName = RearAxleWeight, expandByDefault = False}
                        , {filterName = FleetCode, expandByDefault = False}
                        , {filterName = TruckStatus, expandByDefault = False}
                        , {filterName = SpecialFinancing, expandByDefault = False}
                        , {filterName = InventoryAge, expandByDefault = False}
                        , {filterName = OwningBranch, expandByDefault = False}] 

allRangeFilterTypesMasterList = 
                        [ Price
                        , EngineHP
                        , SleeperInches
                        , WheelBase
                        , Mileage
                        , FrontAxleWeight
                        , RearAxleWeight
                        , InventoryAge
                        ]

allRangeFilterTypesKeyValueParis = 
                        [ 
                          ("Price",Price)
                        , ("EngineHP",EngineHP)
                        , ("SleeperInches",SleeperInches)
                        , ("WheelBase", WheelBase)
                        , ("Mileage",Mileage)
                        , ("FrontAxleWeight", FrontAxleWeight)
                        , ("RearAxleWeight", RearAxleWeight)
                        , ("InventoryAge", InventoryAge)
                        ]

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
        engineMakeFilters = Array.empty,
        transTypeFilters = Array.empty,
        suspensionFilters = Array.empty,
        bodyTypeFilters = Array.empty,
        rearAxleTypeFilters = Array.empty,
        priceFilters = Array.empty,
        engineHPFilters = Array.empty,
        sleeperInchesFilters = Array.empty,
        wheelBaseFilters = Array.empty,
        mileageFilters = Array.empty,
        frontAxleWeightFilters = Array.empty,
        rearAxleWeightFilters = Array.empty,
        fleetCodeFilters = Array.empty,
        truckStatusFilters = Array.empty,
        specialFinancingFilters = Array.empty,
        inventoryAgeFilters = Array.empty,
        owningBranchFilters = Array.empty,
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