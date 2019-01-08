module BusinessFunctions.SearchFilterFunctions exposing (..)

import Model exposing (..)
import Array exposing (..)
import List.Extra exposing (..)
import List.Unique exposing (..)
import Maybe.Extra exposing (..)
 
resetFilters filters = 
        Array.map(\sf -> {sf | userAction = False } ) filters

anyFilterApplied uiModel =
        List.length (uiModel.selectedFilterBullets) > 0

getSelectedFilterBulletsByFilterCategory filterCategory selectedFilterList =
        (Array.fromList <| List.filter (\sf -> sf.filterCategory == filterCategory)  selectedFilterList)
         
getSelectedSearchFilterKeys searchFilters =
    searchFilters 
        |> Array.map (\sf -> sf.searchFilterKey) 
        |> Array.toList

getSelectedSearchFilterExtraData searchFilters =
    searchFilters 
        |> Array.map (\sf -> sf.searchFilterExtraData) 
        |> Array.toList

isGivenValueMatchesWithSelectedFilters value searchFilters  = 
        getSelectedSearchFilterKeys searchFilters
            |> notMember (String.trim value)
            |> not

isGivenValueMatchesWithSelectedRangeFilters value searchFilters  = 
        getSelectedSearchFilterExtraData searchFilters
                |> List.filter 
                                (
                                    \extraDataValue ->
                                        getMinMaxValue extraDataValue
                                            |> (\(minValue,maxValue) ->   value >= minValue && value <= maxValue)
                                )
                |> (\filteredList -> List.length filteredList > 0)

returnPrevOrCurrentlyFilteredTrucks prevFilterdTruckList currentFilteredTruckList =
        if List.length currentFilteredTruckList > 0 then
                currentFilteredTruckList
        else
                prevFilterdTruckList

filterBySingleValue (selectedFilters, trucksList) filterCategory uiModel =
        (find (\filterMetaData -> filterMetaData.filterName == filterCategory ) partialSearchFiltersMetadata )
                |> Maybe.map 
                        (\fltr -> 
                                List.filter 
                                        (
                                                (\t -> fltr.searchTrucksFunction t
                                                        (getSelectedFilterBulletsByFilterCategory filterCategory selectedFilters) )
                                        ) 
                                trucksList
                                        |> returnPrevOrCurrentlyFilteredTrucks trucksList
                                        |> (\trks -> (selectedFilters, trks, uiModel))
                        )
                |> Maybe.withDefault (selectedFilters, trucksList, uiModel)

partialSearchFiltersMetadata = 
    [
         {filterName = FleetCode,        filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.fleetCode) ,                   displayText = "Fleet Code",        pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | fleetCodeFilters        = mfArr}),  filters = \uiModel -> uiModel.fleetCodeFilters}       
        ,{filterName = SalesStatus,      filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.salesStatus) ,                 displayText = "Sales Status",      pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | salesStatusFilters      = mfArr}),  filters = \uiModel -> uiModel.salesStatusFilters}     
        ,{filterName = TruckType,        filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.truckType) ,                   displayText = "Truck Status",      pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | truckTypeFilters        = mfArr}),  filters = \uiModel -> uiModel.truckTypeFilters}       
        ,{filterName = SpecialFinancing, filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.specialFinancing) ,            displayText = "Special Financing", pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | specialFinancingFilters = mfArr}),  filters = \uiModel -> uiModel.specialFinancingFilters}
        ,{filterName = Year,             filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.year) ,                        displayText = "Year",              pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | yearFilters             = mfArr}),  filters = \uiModel -> uiModel.yearFilters}            
        ,{filterName = Make,             filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.make) ,                        displayText = "Make",              pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | makeFilters             = mfArr}),  filters = \uiModel -> uiModel.makeFilters}            
        ,{filterName = MakeModel,        filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.model) ,                       displayText = "Model",             pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | modelFilters            = mfArr}),  filters = \uiModel -> uiModel.modelFilters}           
        ,{filterName = SleeperRoof,      filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.sleeperRoof) ,                 displayText = "Sleeper Roof",      pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | sleeperRoofFilters      = mfArr}),  filters = \uiModel -> uiModel.sleeperRoofFilters}     
        ,{filterName = SleeperBunk,      filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.sleeperBunk) ,                 displayText = "Sleeper Bunk",      pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | sleeperBunkFilters      = mfArr}),  filters = \uiModel -> uiModel.sleeperBunkFilters}     
        ,{filterName = EngineMake,       filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.engineMake) ,                  displayText = "Engine",            pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | engineMakeFilters       = mfArr}),  filters = \uiModel -> uiModel.engineMakeFilters}      
        ,{filterName = TransType,        filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.transType) ,                   displayText = "Transmission",      pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | transTypeFilters        = mfArr}),  filters = \uiModel -> uiModel.transTypeFilters}       
        ,{filterName = Suspension,       filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.suspension) ,                  displayText = "Suspension",        pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | suspensionFilters       = mfArr}),  filters = \uiModel -> uiModel.suspensionFilters}      
        ,{filterName = RearAxleType,     filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.rearAxleType) ,                displayText = "Rear Axle Type",    pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | rearAxleTypeFilters     = mfArr}),  filters = \uiModel -> uiModel.rearAxleTypeFilters}    
        ,{filterName = LocationName,     filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.locationName) ,                displayText = "Location Name",     pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | locationNameFilters     = mfArr}),  filters = \uiModel -> uiModel.locationNameFilters}    
        ,{filterName = OwningBranch,     filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.owningBranch) ,                displayText = "Owning Branch",     pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | owningBranchFilters     = mfArr}),  filters = \uiModel -> uiModel.owningBranchFilters}           
        ,{filterName = BodyType,         filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.bodyType) ,                    displayText = "Body Type",         pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | bodyTypeFilters         = mfArr}),  filters = \uiModel -> uiModel.bodyTypeFilters}        
        ,{filterName = APU,              filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.apu) ,                         displayText = "APU",               pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | apuFilters              = mfArr}),  filters = \uiModel -> uiModel.apuFilters}             
        ,{filterName = CDL,              filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.cdl) ,                         displayText = "CDL",               pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | cdlFilters              = mfArr}),  filters = \uiModel -> uiModel.cdlFilters}             
        ,{filterName = Photo,            filterStyle = SingleValue ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.hasPhoto) ,                    displayText = "Photo",             pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | photoFilters            = mfArr}),  filters = \uiModel -> uiModel.photoFilters}           
        ,{filterName = BrakeType,        filterStyle = SingleValue  , searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.brakeType) ,                   displayText = "Brake Type",        pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | brakeTypeFilters        = mfArr}),  filters = \uiModel -> uiModel.brakeTypeFilters}  
        ,{filterName = ExhaustType,      filterStyle = SingleValue  , searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedFilters t.exhaustType) ,                 displayText = "Exhaust Type",      pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | exhaustTypeFilters      = mfArr}),  filters = \uiModel -> uiModel.exhaustTypeFilters}  

        ,{filterName = Price,            filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.price) ,                  displayText = "Price",             pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | priceFilters            = mfArr}),  filters = \uiModel -> uiModel.priceFilters}           
        ,{filterName = SleeperInches,    filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.sleeperInches) ,          displayText = "Sleeper Size",      pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | sleeperInchesFilters    = mfArr}),  filters = \uiModel -> uiModel.sleeperInchesFilters}   
        ,{filterName = EngineHP,         filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.engineHP) ,               displayText = "HP",                pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | engineHPFilters         = mfArr}),  filters = \uiModel -> uiModel.engineHPFilters}        
        ,{filterName = WheelBase,        filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.wheelBase) ,              displayText = "Wheel Base",        pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | wheelBaseFilters        = mfArr}),  filters = \uiModel -> uiModel.wheelBaseFilters}       
        ,{filterName = FrontAxleWeight,  filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.frontAxleWeight) ,        displayText = "Front Axle Weight", pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | frontAxleWeightFilters  = mfArr}),  filters = \uiModel -> uiModel.frontAxleWeightFilters} 
        ,{filterName = RearAxleWeight,   filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.rearAxleWeight) ,         displayText = "Rear Axle Weight",  pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | rearAxleWeightFilters   = mfArr}),  filters = \uiModel -> uiModel.rearAxleWeightFilters}  
        ,{filterName = InventoryAge,     filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.inventoryAge) ,           displayText = "Inventory Age",     pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | inventoryAgeFilters     = mfArr}),  filters = \uiModel -> uiModel.inventoryAgeFilters}    
        ,{filterName = Mileage,          filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.mileage) ,                displayText = "Mileage",           pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | mileageFilters          = mfArr}),  filters = \uiModel -> uiModel.mileageFilters}         

        ,{filterName = RearAxleRatio,    filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.rearAxleRatio) ,          displayText = "Rear Axle Ratio",   pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | rearAxleRatioFilters    = mfArr}),  filters = \uiModel -> uiModel.rearAxleRatioFilters}         
        ,{filterName = RearWheelSize,    filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.rearWheelSize) ,          displayText = "Rear Wheel Size",   pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | rearWheelSizeFilters    = mfArr}),  filters = \uiModel -> uiModel.rearWheelSizeFilters}         
        ,{filterName = FrontWheelSize,   filterStyle = RangeValue  ,  searchTrucksFunction = (\t -> isGivenValueMatchesWithSelectedRangeFilters t.frontWheelSize) ,         displayText = "Front Wheel Size",  pushModifiedFilterListBackInToUIModel = (\uiModel mfArr -> {uiModel | frontWheelSizeFilters   = mfArr}),  filters = \uiModel -> uiModel.frontWheelSizeFilters}         
    ]

executeFilterFunc filterMetaData (sfBullets, trks, uiModel)  =
        filterBySingleValue (sfBullets, trks) filterMetaData.filterName uiModel
        
rebuildFilters filterStyle filterCategory filters (selectedFilterBullets, finalFilteredTrucks, uiModel) =
        buildSearchFilterValueRecordList filterStyle filterCategory filters finalFilteredTrucks
                |> Array.map
                        (\sf ->
                                findMatchAndSetUserAction (Array.fromList selectedFilterBullets) sf
                        )

findMatchAndSetUserAction filters sf =
                        filters
                                |> Array.filter(\uiSF -> uiSF.searchFilterKey == sf.searchFilterKey && uiSF.filterCategory == sf.filterCategory)
                                |> Array.toList
                                |> List.head
                                |> (\headItem -> 
                                        case headItem of 
                                                Just val -> val
                                                Nothing -> sf)
                                |> (\headItem -> {sf | userAction = headItem.userAction} )

rebuildSearchFiltersBasedOnCurrentSearchCriteria : Model -> UIModel -> UIModel
rebuildSearchFiltersBasedOnCurrentSearchCriteria model uiModel =
        let 
                applyAllFiltersExcept filterCategory filters  filterStyle =
                        List.foldl
                                executeFilterFunc
                                (uiModel.selectedFilterBullets, model.truckList, uiModel) 
                                (List.filter (\filterMetaData -> filterMetaData.filterName /= filterCategory ) partialSearchFiltersMetadata )
                                        |> rebuildFilters filterStyle filterCategory filters
                
                executeRegularAndRangeFilterFunc filterMeta currentUIModel =
                        filterMeta.pushModifiedFilterListBackInToUIModel currentUIModel (applyAllFiltersExcept  filterMeta.filterName (filterMeta.filters currentUIModel) filterMeta.filterStyle)

                newUIModel = 
                        List.foldl
                                executeRegularAndRangeFilterFunc
                                uiModel
                                partialSearchFiltersMetadata
        in
                newUIModel

applySearchFilters: Model -> UIModel -> List Truck
applySearchFilters model uiModel =
    let
        filterdTruckList  = 
                List.foldl
                        executeFilterFunc 
                        (uiModel.selectedFilterBullets, model.truckList, uiModel) 
                        partialSearchFiltersMetadata
                                |> \(_, finalFilteredTrucks, _) -> finalFilteredTrucks
    in
        filterdTruckList

getMinMaxValue rangeString =
        let
                minmaxValues = String.split "-" rangeString
                minValue =     
                        case List.head <| minmaxValues of -- gives first element in the list
                        Just strMinVal -> case String.toFloat strMinVal of 
                                                Just minVal -> minVal
                                                Nothing -> 0                    
                        Nothing -> 0
                maxValue =
                        case List.head <| List.reverse minmaxValues of -- gives last element in the list -- 2nd style
                                Just strMaxVal -> case String.toFloat strMaxVal of 
                                                Just maxVal -> maxVal
                                                Nothing -> 0     
                                Nothing -> 0     
        in
                (minValue, maxValue)

-- simple type compares
desendingOrder a b =
    case compare a b of
        LT -> GT
        EQ -> EQ
        GT -> LT

filterEmptyValuesFromList : List String -> List String
filterEmptyValuesFromList  searchFilterList =
    List.filter (
                    \str -> 
                        str
                            |> String.trim
                            |> String.isEmpty
                            |> not 
                )
                searchFilterList

applyExtraOnSearchFilters  : SortOrder -> List String -> Array String
applyExtraOnSearchFilters sortOrder searchFilterKeyValues =
    filterDuplicates searchFilterKeyValues
        |> filterEmptyValuesFromList
        |> (if sortOrder == SortASC then 
                List.sort 
            else 
                List.sortWith desendingOrder)
        |> Array.fromList

buildSearchFilterValueList : SearchFilterStyle -> SearchFilterCustomType ->  Array SearchFilterType -> List Truck -> Array SearchFilterType
buildSearchFilterValueList filterStyle searchFilterCustomType searchFilterTypes trucks =
        if filterStyle == SingleValue then
                let
                
                        sfMetaData =  
                                find (\sfMeta -> sfMeta.filterName == searchFilterCustomType) regularSearchFiltersInitialExpandState
                                        |> Maybe.map (\sfMeta -> sfMeta)
                                        -- the below condition should never happen unless you misspell in metadata list in model.elm file
                                        |> Maybe.withDefault {filterName = defaultSearchFiltersMetadata.filterName, truckFieldFunction = defaultSearchFiltersMetadata.truckFieldFunction, expandByDefault = defaultSearchFiltersMetadata.expandByDefault }

                        fieldFunc = Tuple.first sfMetaData.truckFieldFunction
                        fieldcompareFunc = Tuple.second sfMetaData.truckFieldFunction
                in    
                        (fieldFunc trucks)
                                |> (if searchFilterCustomType == Year then applyExtraOnSearchFilters SortDSC else applyExtraOnSearchFilters SortASC)
                                |> (\sfArray -> 
                                                Array.indexedMap (\index sf -> 
 
                                                        SearchFilterType
                                                                        index 
                                                                        sf 
                                                                        sf
                                                                        False
                                                                        (List.length <| ( List.filter (fieldcompareFunc sf) trucks ) ) 
                                                                        searchFilterCustomType
                                                )
                                                sfArray
                                ) 
        else
                let
                        sfMetaData =  
                                find (\sfMeta -> sfMeta.filterName == searchFilterCustomType) rangeSearchFiltersInitialExpandState
                                        |> Maybe.map (\sfMeta -> sfMeta)
                                        -- the below condition should never happen unless you misspell in metadata list in model.elm file
                                        |> Maybe.withDefault {filterName = defaultSearchFiltersMetadata.filterName, filterNameString = defaultSearchFiltersMetadata.filterNameString, truckRangeFieldFunction = defaultSearchFiltersMetadata.truckRangeFieldFunction, expandByDefault = defaultSearchFiltersMetadata.expandByDefault }

                        fieldcompareFunc = sfMetaData.truckRangeFieldFunction
                in    
                       createRangeFilters       
                                                searchFilterTypes
                                                searchFilterCustomType 
                                                (\minValue maxValue ->
                                                        (List.length <| List.filter (fieldcompareFunc minValue maxValue) trucks) 
                                                )

createRangeFilters searchFilterTypes searchFilterCustomType filterCompareCheckFunc = 
        Array.indexedMap
                         (\index range -> 

                            let
                                minmaxValue = getMinMaxValue range.searchFilterExtraData     
                                minValue = Tuple.first minmaxValue
                                maxValue = Tuple.second minmaxValue
                            in
                                --using Constructor style
                                SearchFilterType        index
                                                        range.searchFilterKey 
                                                        range.searchFilterExtraData 
                                                        False
                                                        (filterCompareCheckFunc minValue maxValue)
                                                        searchFilterCustomType

                         )
                        searchFilterTypes

buildSearchFilterValueRecordList : SearchFilterStyle -> SearchFilterCustomType -> Array SearchFilterType -> List Truck -> Array SearchFilterType
buildSearchFilterValueRecordList searchFilterStyle searchFilterCustomType searchFilterTypes trucks =
    buildSearchFilterValueList searchFilterStyle searchFilterCustomType searchFilterTypes trucks