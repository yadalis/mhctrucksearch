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
        ,truckTypeFilters : Array SearchFilterType
        ,priceFilters : Array SearchFilterType
        ,engineHPFilters : Array SearchFilterType
        ,sleeperInchesFilters : Array SearchFilterType
        ,wheelBaseFilters : Array SearchFilterType
        ,mileageFilters : Array SearchFilterType
        ,frontAxleWeightFilters : Array SearchFilterType
        ,rearAxleWeightFilters : Array SearchFilterType
        ,fleetCodeFilters : Array SearchFilterType
        ,specialFinancingFilters : Array SearchFilterType
        ,inventoryAgeFilters : Array SearchFilterType        
        ,owningBranchFilters : Array SearchFilterType
        ,apuFilters : Array SearchFilterType
        ,cdlFilters : Array SearchFilterType
        ,photoFilters : Array SearchFilterType
        ,locationNameFilters : Array SearchFilterType
        ,brakeTypeFilters : Array SearchFilterType
        ,exhaustTypeFilters : Array SearchFilterType
        ,rearAxleRatioFilters : Array SearchFilterType
        ,rearWheelSizeFilters : Array SearchFilterType
        ,frontWheelSizeFilters : Array SearchFilterType
        ,selectedFilterBullets : List SearchFilterType
        ,expandCollapseSearchFilterStates : Array SearchFilterState
        --,collapseAllChecked : Bool
        ,showDropdown : Bool
        ,showLoader : Bool
        ,currentSortBy : SortBy
        --,showAppraisedTrucks : Bool
        ,workWithAppraisedTrucks : Bool
        ,workWithNewTrucks : Bool
    }

type alias SearchFilterState =
    {
        index : Int
        ,searchFilterCustomType : SearchFilterCustomType
        ,userAction : Bool
    }

-- type alias SearchFilterBulletState =
--     {
--         index : Int
--         ,searchFilterCustomType : SearchFilterCustomType
--         ,userAction : Bool
--         ,realtedFilterIndex : Int
--     }

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
    | SortDSC

type SearchFilterStyle
    = SingleValue
    | RangeValue

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
        ,filteredTruckList = []
        ,pagedTruckList = []
        ,currentPageNumber = 1
    }


--searchFiltersMetadata : UIModel -> List  {filterName : SearchFilterCustomType, displayText : String, filters : Array SearchFilterType, expandByDefault : Bool }
regularSearchFiltersInitialExpandState = 
    [
          {filterName = FleetCode,          truckFieldFunction =  (List.map .fleetCode,        (\sf t -> String.trim t.fleetCode        == sf) )  ,expandByDefault = False}
        , {filterName = SalesStatus,        truckFieldFunction =  (List.map .salesStatus,      (\sf t -> String.trim t.salesStatus      == sf) )  ,expandByDefault = True}
        , {filterName = TruckType,          truckFieldFunction =  (List.map .truckType,        (\sf t -> String.trim t.truckType        == sf) )  ,expandByDefault = False}
        , {filterName = SpecialFinancing,   truckFieldFunction =  (List.map .specialFinancing, (\sf t -> String.trim t.specialFinancing == sf) )  ,expandByDefault = False}
        , {filterName = Year,               truckFieldFunction =  (List.map .year,             (\sf t -> String.trim t.year             == sf) )  ,expandByDefault = False}
        , {filterName = Make,               truckFieldFunction =  (List.map .make,             (\sf t -> String.trim t.make             == sf) )  ,expandByDefault = True}
        , {filterName = MakeModel,          truckFieldFunction =  (List.map .model,            (\sf t -> String.trim t.model            == sf) )  ,expandByDefault = False}         
        , {filterName = SleeperRoof,        truckFieldFunction =  (List.map .sleeperRoof,      (\sf t -> String.trim t.sleeperRoof      == sf) )  ,expandByDefault = False}
        , {filterName = SleeperBunk,        truckFieldFunction =  (List.map .sleeperBunk,      (\sf t -> String.trim t.sleeperBunk      == sf) )  ,expandByDefault = False}
        , {filterName = EngineMake,         truckFieldFunction =  (List.map .engineMake,       (\sf t -> String.trim t.engineMake       == sf) )  ,expandByDefault = False}
        , {filterName = TransType,          truckFieldFunction =  (List.map .transType,        (\sf t -> String.trim t.transType        == sf) )  ,expandByDefault = False}
        , {filterName = Suspension,         truckFieldFunction =  (List.map .suspension,       (\sf t -> String.trim t.suspension       == sf) )  ,expandByDefault = False}
        , {filterName = RearAxleType,       truckFieldFunction =  (List.map .rearAxleType,     (\sf t -> String.trim t.rearAxleType     == sf) )  ,expandByDefault = False}
        , {filterName = LocationName,       truckFieldFunction =  (List.map .locationName,     (\sf t -> String.trim t.locationName     == sf) )  ,expandByDefault = False}
        , {filterName = OwningBranch,       truckFieldFunction =  (List.map .owningBranch,     (\sf t -> String.trim t.owningBranch     == sf) )  ,expandByDefault = False}
        , {filterName = BodyType,           truckFieldFunction =  (List.map .bodyType,         (\sf t -> String.trim t.bodyType         == sf) )  ,expandByDefault = False}
        , {filterName = APU,                truckFieldFunction =  (List.map .apu,              (\sf t -> String.trim t.apu              == sf) )  ,expandByDefault = False}
        , {filterName = CDL,                truckFieldFunction =  (List.map .cdl,              (\sf t -> String.trim t.cdl              == sf) )  ,expandByDefault = False}
        , {filterName = Photo,              truckFieldFunction =  (List.map .hasPhoto,         (\sf t -> String.trim t.hasPhoto         == sf) )  ,expandByDefault = False}
        , {filterName = BrakeType,          truckFieldFunction =  (List.map .brakeType,        (\sf t -> String.trim t.brakeType        == sf) )  ,expandByDefault = False}
        , {filterName = ExhaustType,        truckFieldFunction =  (List.map .exhaustType,      (\sf t -> String.trim t.exhaustType      == sf) )  ,expandByDefault = False}

    ]

rangeSearchFiltersInitialExpandState = 
    [
          {filterName = Price,           filterNameString = "Price",            truckRangeFieldFunction = (\minValue maxValue t -> t.price >= minValue && t.price <= maxValue),                         expandByDefault = True}
        , {filterName = SleeperInches,   filterNameString = "SleeperInches",    truckRangeFieldFunction = (\minValue maxValue t -> t.sleeperInches >= minValue && t.sleeperInches <= maxValue),         expandByDefault = False}
        , {filterName = EngineHP,        filterNameString = "EngineHP",         truckRangeFieldFunction = (\minValue maxValue t -> t.engineHP >= minValue && t.engineHP <= maxValue),                   expandByDefault = False}
        , {filterName = WheelBase,       filterNameString = "WheelBase",        truckRangeFieldFunction = (\minValue maxValue t -> t.wheelBase >= minValue && t.wheelBase <= maxValue),                 expandByDefault = False}
        , {filterName = FrontAxleWeight, filterNameString = "FrontAxleWeight",  truckRangeFieldFunction = (\minValue maxValue t -> t.frontAxleWeight >= minValue && t.frontAxleWeight <= maxValue),     expandByDefault = False}
        , {filterName = RearAxleWeight,  filterNameString = "RearAxleWeight",   truckRangeFieldFunction = (\minValue maxValue t -> t.rearAxleWeight >= minValue && t.rearAxleWeight <= maxValue),       expandByDefault = False}
        , {filterName = InventoryAge,    filterNameString = "InventoryAge",     truckRangeFieldFunction = (\minValue maxValue t -> t.inventoryAge >= minValue && t.inventoryAge <= maxValue),           expandByDefault = False}
        , {filterName = Mileage,         filterNameString = "Mileage",          truckRangeFieldFunction = (\minValue maxValue t -> t.mileage >= minValue && t.mileage <= maxValue),                     expandByDefault = False}
        , {filterName = RearAxleRatio,   filterNameString = "RearAxleRatio",    truckRangeFieldFunction = (\minValue maxValue t -> t.rearAxleRatio >= minValue && t.rearAxleRatio <= maxValue),         expandByDefault = False}
        , {filterName = RearWheelSize,   filterNameString = "RearWheelSize",    truckRangeFieldFunction = (\minValue maxValue t -> t.rearWheelSize >= minValue && t.rearWheelSize <= maxValue),         expandByDefault = False}
        , {filterName = FrontWheelSize,  filterNameString = "FrontWheelSize",   truckRangeFieldFunction = (\minValue maxValue t -> t.frontWheelSize >= minValue && t.frontWheelSize <= maxValue),       expandByDefault = False}
    ]

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
        truckTypeFilters = Array.empty,
        fleetCodeFilters = Array.empty,
        specialFinancingFilters = Array.empty,
        inventoryAgeFilters = Array.empty,
        owningBranchFilters = Array.empty,
        apuFilters = Array.empty,
        cdlFilters = Array.empty,
        photoFilters = Array.empty,
        locationNameFilters = Array.empty,
        brakeTypeFilters = Array.empty,
        rearAxleRatioFilters = Array.empty,
        exhaustTypeFilters = Array.empty,
        rearWheelSizeFilters = Array.empty,        
        frontWheelSizeFilters = Array.empty,
        selectedFilterBullets = [],
        expandCollapseSearchFilterStates = 
                Array.fromList <|
                        List.concat
                        [
                            (List.indexedMap 
                                        (
                                            \index searchFilterTypeRecord ->
                                                        {index = index,searchFilterCustomType = searchFilterTypeRecord.filterName, userAction = searchFilterTypeRecord.expandByDefault}
                                        )
                            regularSearchFiltersInitialExpandState)
                            ,
                            (List.indexedMap 
                                        (
                                            \index searchFilterTypeRecord ->
                                                        {index = index +  List.length regularSearchFiltersInitialExpandState,searchFilterCustomType = searchFilterTypeRecord.filterName, userAction = searchFilterTypeRecord.expandByDefault}
                                        )
                            rangeSearchFiltersInitialExpandState)
                        ]
                        ,
        showDropdown = False,
        showLoader = False,
        currentSortBy = MakeAtoZ,
        workWithAppraisedTrucks = False,
        workWithNewTrucks = False
    }