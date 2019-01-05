module BusinessFunctions.SearchFilterFunctions exposing (..)

import Model exposing (..)
import Array exposing (..)
import SearchFilterViews.SearchFilter exposing (..)
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

filterBySalesStatus (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.salesStatus (getSelectedFilterBulletsByFilterCategory SalesStatus selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByYear (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.year (getSelectedFilterBulletsByFilterCategory Year selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByMake (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.make (getSelectedFilterBulletsByFilterCategory Make selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))                

filterByModel (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.model (getSelectedFilterBulletsByFilterCategory MakeModel selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySleeperRoof (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.sleeperRoof (getSelectedFilterBulletsByFilterCategory SleeperRoof selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySleeperBunk (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.sleeperBunk (getSelectedFilterBulletsByFilterCategory SleeperBunk selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByEngineMake (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.engineMake (getSelectedFilterBulletsByFilterCategory EngineMake selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByTransType (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.transType (getSelectedFilterBulletsByFilterCategory TransType selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySuspension (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.suspension (getSelectedFilterBulletsByFilterCategory Suspension selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByBodyType (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.bodyType (getSelectedFilterBulletsByFilterCategory BodyType selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByFleetCode (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.fleetCode (getSelectedFilterBulletsByFilterCategory FleetCode selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySpecialFinancing (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.specialFinancing (getSelectedFilterBulletsByFilterCategory SpecialFinancing selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByOwningBranch (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.owningBranch (getSelectedFilterBulletsByFilterCategory OwningBranch selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByLocationName (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.locationName (getSelectedFilterBulletsByFilterCategory LocationName selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByRearAxleType (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.rearAxleType (getSelectedFilterBulletsByFilterCategory RearAxleType selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByTruckType (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.truckType (getSelectedFilterBulletsByFilterCategory TruckType selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByAPU (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.apu (getSelectedFilterBulletsByFilterCategory APU selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByCDL (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.cdl (getSelectedFilterBulletsByFilterCategory CDL selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByPhoto (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.hasPhoto (getSelectedFilterBulletsByFilterCategory Photo selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

----------Range filters

filterByPrice (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.price (getSelectedFilterBulletsByFilterCategory Price selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByEngineHP (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.engineHP (getSelectedFilterBulletsByFilterCategory EngineHP selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySleeperInches (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.sleeperInches (getSelectedFilterBulletsByFilterCategory SleeperInches selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByWheelBase (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.wheelBase (getSelectedFilterBulletsByFilterCategory WheelBase selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByMileage (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.mileage (getSelectedFilterBulletsByFilterCategory Mileage selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByFrontAxleWeight (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.frontAxleWeight (getSelectedFilterBulletsByFilterCategory FrontAxleWeight selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByRearAxleWeight (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.rearAxleWeight (getSelectedFilterBulletsByFilterCategory RearAxleWeight selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterByInventoryAge (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.inventoryAge (getSelectedFilterBulletsByFilterCategory InventoryAge selectedFilters)) trucksList
                |> returnPrevOrCurrentlyFilteredTrucks trucksList
                |> (\trks -> (selectedFilters, trks))

filterFunctionsList = [
                (SalesStatus, filterBySalesStatus)
                , (Year,filterByYear)
                , (Make,filterByMake)
                , (MakeModel,filterByModel)
                , (SleeperRoof,filterBySleeperRoof)
                , (SleeperBunk,filterBySleeperBunk )
                , (EngineMake,filterByEngineMake )
                , (TransType,filterByTransType )
                , (Suspension,filterBySuspension )
                , (BodyType,filterByBodyType )
                , (RearAxleType,filterByRearAxleType)
                , (TruckType,filterByTruckType )
                , (FleetCode,filterByFleetCode )
                , (SpecialFinancing,filterBySpecialFinancing)
                , (OwningBranch,filterByOwningBranch )
                , (LocationName,filterByLocationName )
                , (APU,filterByAPU )
                , (CDL,filterByCDL )
                , (Photo,filterByPhoto)
                --range filters
                , (Price,filterByPrice)
                , (EngineHP,filterByEngineHP)
                , (SleeperInches,filterBySleeperInches)
                , (WheelBase,filterByWheelBase )
                , (Mileage,filterByMileage )
                , (FrontAxleWeight,filterByFrontAxleWeight)
                , (RearAxleWeight,filterByRearAxleWeight )
                , (InventoryAge,filterByInventoryAge )
        ]

executeFilterFunc (fnCategory, fn) (sfBullets, trks) =
        fn (sfBullets, trks)

rebuildFilters filterCategory filters (selectedFilterBullets, finalFilteredTrucks) =
        buildSearchFilterValueRecordList filterCategory filters finalFilteredTrucks     
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
                
                applyAllFiltersExcept filterCategory filters =
                        List.foldl
                                executeFilterFunc 
                                (uiModel.selectedFilterBullets, model.truckList) 
                                (List.filter (\(fltrCategory, fn) -> fltrCategory /= filterCategory ) filterFunctionsList)
                                        |> rebuildFilters filterCategory filters
                    
                newUIModel =  
                        {
                                uiModel |
                                                fleetCodeFilters = applyAllFiltersExcept FleetCode uiModel.fleetCodeFilters
                                                , salesStatusFilters = applyAllFiltersExcept SalesStatus uiModel.salesStatusFilters
                                                , truckTypeFilters = applyAllFiltersExcept TruckType uiModel.truckTypeFilters
                                                , specialFinancingFilters = applyAllFiltersExcept SpecialFinancing uiModel.specialFinancingFilters
                                                , yearFilters = applyAllFiltersExcept Year uiModel.yearFilters
                                                , makeFilters = applyAllFiltersExcept Make uiModel.makeFilters
                                                , modelFilters = applyAllFiltersExcept MakeModel uiModel.modelFilters
                                                , priceFilters = applyAllFiltersExcept Price uiModel.priceFilters
                                                , sleeperInchesFilters = applyAllFiltersExcept SleeperInches uiModel.sleeperInchesFilters
                                                , sleeperRoofFilters = applyAllFiltersExcept SleeperRoof uiModel.sleeperRoofFilters
                                                , sleeperBunkFilters = applyAllFiltersExcept SleeperBunk uiModel.sleeperBunkFilters
                                                , engineMakeFilters = applyAllFiltersExcept EngineMake uiModel.engineMakeFilters
                                                , engineHPFilters = applyAllFiltersExcept EngineHP uiModel.engineHPFilters
                                                , transTypeFilters = applyAllFiltersExcept TransType uiModel.transTypeFilters
                                                , suspensionFilters = applyAllFiltersExcept Suspension uiModel.suspensionFilters
                                                , wheelBaseFilters = applyAllFiltersExcept WheelBase uiModel.wheelBaseFilters
                                                , frontAxleWeightFilters = applyAllFiltersExcept FrontAxleWeight uiModel.frontAxleWeightFilters
                                                , rearAxleTypeFilters = applyAllFiltersExcept RearAxleType uiModel.rearAxleTypeFilters
                                                , rearAxleWeightFilters = applyAllFiltersExcept RearAxleWeight uiModel.rearAxleWeightFilters
                                                , inventoryAgeFilters = applyAllFiltersExcept InventoryAge uiModel.inventoryAgeFilters
                                                , locationNameFilters = applyAllFiltersExcept LocationName uiModel.locationNameFilters
                                                , owningBranchFilters = applyAllFiltersExcept OwningBranch uiModel.owningBranchFilters
                                                , mileageFilters = applyAllFiltersExcept Mileage uiModel.mileageFilters
                                                , bodyTypeFilters = applyAllFiltersExcept BodyType uiModel.bodyTypeFilters
                                                , apuFilters = applyAllFiltersExcept APU uiModel.apuFilters
                                                , cdlFilters = applyAllFiltersExcept CDL uiModel.cdlFilters
                                                , photoFilters = applyAllFiltersExcept Photo uiModel.photoFilters
                        }
        in
                newUIModel

applySearchFilters: Model -> UIModel -> List Truck
applySearchFilters model uiModel =
    let
        filterdTruckList  = 
                List.foldl
                        executeFilterFunc 
                        (uiModel.selectedFilterBullets, model.truckList) 
                        filterFunctionsList
                                |> \(_, finalFilteredTrucks) -> finalFilteredTrucks
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
applyExtraOnSearchFilters sortOrder searchFilterKeyValue =
    filterDuplicates searchFilterKeyValue
        |> filterEmptyValuesFromList
        |> (if sortOrder == SortASC then 
                List.sort 
            else 
                List.sortWith desendingOrder)
        |> Array.fromList

buildSearchFilterValueList : SearchFilterCustomType ->  Array SearchFilterType -> List Truck -> Array SearchFilterType
buildSearchFilterValueList searchFilterCustomType searchFilterTypes trucks =
    case searchFilterCustomType of
        SalesStatus -> 
            --List.map (\t -> t.salesStatus) trucks
            List.map .salesStatus trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                               SearchFilterType index sf "EXD" 
                                               --(if String.toLower sf == "available" then True else False)
                                               False
                                               (List.length <| (List.filter (\t -> String.trim t.salesStatus == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    ) 

        Year -> 
            --List.map (\t -> t.year) trucks
            List.map .year trucks
                |> applyExtraOnSearchFilters SortDSC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.year == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )
                
        Make -> 
            --List.map (\t -> t.make) trucks
            List.map .make trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf ->  
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.make == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                

        MakeModel -> 
            --List.map (\t -> t.model) trucks
            List.map .model trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.model == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                

        SleeperRoof -> 
            --List.map (\t -> t.sleeperRoof) trucks
            List.map .sleeperRoof trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.sleeperRoof == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        SleeperBunk ->             
            List.map .sleeperBunk trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.sleeperBunk == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    ) 
                
        EngineMake ->             
            List.map .engineMake trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.engineMake == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                                
                
        TransType ->             
            List.map .transType trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.transType == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                                               
                
        Suspension ->             
            List.map .suspension trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.suspension == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                                                              
                
        BodyType ->             
            List.map .bodyType trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.bodyType == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                        
                
        RearAxleType ->             
            List.map .rearAxleType trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.rearAxleType == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        TruckType ->             
            List.map .truckType trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                let
                                                    trimmedsfValue = String.trim sf
                                                    displayValue = 
                                                                    if trimmedsfValue == "I" then
                                                                         "Inventory"
                                                                    else if trimmedsfValue == "A" then 
                                                                        "Appraisal"
                                                                    else
                                                                        "Purchase Order"
                                                in
                                                
                                                SearchFilterType index trimmedsfValue displayValue False (List.length <| (List.filter (\t -> String.trim t.truckType == trimmedsfValue) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                

        FleetCode ->        
            List.map .fleetCode trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.fleetCode == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        SpecialFinancing ->        
            List.map .specialFinancing trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.specialFinancing == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )                
                
        OwningBranch ->        
            List.map .owningBranch trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.owningBranch == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )      

        LocationName ->        
            List.map .locationName trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.locationName == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )

        APU ->        
            List.map .apu trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.apu == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )  

        CDL ->        
            List.map .cdl trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.cdl == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )  

        Photo ->        
            List.map .hasPhoto trucks
                |> applyExtraOnSearchFilters SortASC
                |> (\sfArray -> 
                                Array.indexedMap (\index sf -> 
                                                SearchFilterType index sf "EXD" False (List.length <| (List.filter (\t -> String.trim t.hasPhoto == sf) trucks )) searchFilterCustomType
                                )
                                sfArray
                    )  

------------------Range filters 
        Price ->            
                createRangeFilters  searchFilterTypes
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.price >= minValue && t.price <= maxValue) trucks) 
                                    )

        EngineHP ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.engineHP >= minValue && t.engineHP <= maxValue) trucks) 
                                    )

        SleeperInches ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                        -- let
                                        --     y = Debug.log "minmax" [minValue, maxValue]
                                        -- in

                                            (List.length <| List.filter (\t -> t.sleeperInches >= minValue && t.sleeperInches <= maxValue) trucks) 
                                    )

        WheelBase ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.wheelBase >= minValue && t.wheelBase <= maxValue) trucks) 
                                    )

        Mileage ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.mileage >= minValue && t.mileage <= maxValue) trucks) 
                                    )

        FrontAxleWeight ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.frontAxleWeight >= minValue && t.frontAxleWeight <= maxValue) trucks) 
                                    )

        RearAxleWeight ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.rearAxleWeight >= minValue && t.rearAxleWeight <= maxValue) trucks) 
                                    )

        InventoryAge ->
                createRangeFilters  searchFilterTypes 
                                    searchFilterCustomType 
                                    (\minValue maxValue ->
                                            (List.length <| List.filter (\t -> t.inventoryAge >= minValue && t.inventoryAge <= maxValue) trucks) 
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
                                SearchFilterType   index 
                                                        range.searchFilterKey 
                                                        range.searchFilterExtraData 
                                                        False
                                                        (filterCompareCheckFunc minValue maxValue)
                                                        searchFilterCustomType

                         )
                        searchFilterTypes

buildSearchFilterValueRecordList : SearchFilterCustomType -> Array SearchFilterType -> List Truck -> Array SearchFilterType
buildSearchFilterValueRecordList searchFilterCustomType searchFilterTypes trucks =
    buildSearchFilterValueList searchFilterCustomType searchFilterTypes trucks