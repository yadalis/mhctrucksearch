module BusinessFunctions.TruckFunctions exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Model exposing (..)
import Array exposing (..)
import SearchFilterViews.SearchFilter exposing (..)
import List.Extra exposing (..)
import List.Unique exposing (..)
import Maybe.Extra exposing (..)

resetFilters filters = 
        if selectedFiltersCount filters > 0 then
                Array.map(\sf -> {sf | userAction = False } ) filters
        else
                filters

selectedFiltersCount filters = 
        count (\sf -> sf.userAction == True) <| Array.toList filters
        --Array.length <| Array.filter (\sf -> sf.userAction == True) filters

concatAllFilters uiModel =
        List.concat
        [ 
                Array.toList uiModel.salesStatusFilters,
                Array.toList uiModel.yearFilters,
                Array.toList uiModel.makeFilters,
                Array.toList uiModel.modelFilters,
                Array.toList uiModel.sleeperRoofFilters,
                Array.toList uiModel.sleeperBunkFilters,
                Array.toList uiModel.engineMakeFilters,
                Array.toList uiModel.transTypeFilters,
                Array.toList uiModel.suspensionFilters,
                Array.toList uiModel.bodyTypeFilters,
                Array.toList uiModel.rearAxleTypeFilters,
                Array.toList uiModel.truckTypeFilters,
                Array.toList uiModel.fleetCodeFilters,
                Array.toList uiModel.specialFinancingFilters,
                Array.toList uiModel.owningBranchFilters,
                Array.toList uiModel.apuFilters,
                Array.toList uiModel.cdlFilters,
                Array.toList uiModel.photoFilters,
                Array.toList uiModel.priceFilters,
                Array.toList uiModel.engineHPFilters,
                Array.toList uiModel.sleeperInchesFilters,
                Array.toList uiModel.wheelBaseFilters,
                Array.toList uiModel.mileageFilters,
                Array.toList uiModel.frontAxleWeightFilters,
                Array.toList uiModel.rearAxleWeightFilters,
                Array.toList uiModel.inventoryAgeFilters,
                Array.toList uiModel.locationNameFilters
        ]

getSelectedSearchFilterKeys searchFilters =
        searchFilters 
                |> Array.filter (\sf -> sf.userAction == True)
                |> Array.map (\sf -> 
                                --     if sf.filterCategory == TruckType then
                                --         sf.searchFilterExtraData
                                --     else
                                        sf.searchFilterKey
                        ) 
                |> Array.toList

getSelectedSearchFilterExtraData searchFilters =
        searchFilters 
                |> Array.filter (\sf -> sf.userAction == True)
                |> Array.map (\sf -> sf.searchFilterExtraData) 
                |> Array.toList

isGivenValueMatchesWithSelectedFilters value searchFilters  = 
        not <| notMember (String.trim value) <| getSelectedSearchFilterKeys searchFilters

isGivenValueMatchesWithSelectedRangeFilters value searchFilters  = 
        (getSelectedSearchFilterExtraData searchFilters)
                |> List.filter (\extraData ->
                                        getMinMaxValue extraData
                                        |> (\(minValue,maxValue) ->     
                                                value >= minValue && value <= maxValue
                                        )
                )
                |> List.length
                |> (\lng -> if lng > 0 then True else False)
                
returnListWithValues prevFilterdTruckList currentFilteredTruckList =
        if List.length currentFilteredTruckList > 0 then
                currentFilteredTruckList
        else
                prevFilterdTruckList

filterBySalesStatus salesStatusFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.salesStatus salesStatusFilters) trucksList
                |> returnListWithValues trucksList

filterByYear yearFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.year yearFilters) trucksList
                |> returnListWithValues trucksList

filterByMake makeFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.make makeFilters) trucksList
                |> returnListWithValues trucksList

filterByModel modelFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.model modelFilters) trucksList
                |> returnListWithValues trucksList

filterBySleeperRoof sleeperRoofFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.sleeperRoof sleeperRoofFilters) trucksList
                |> returnListWithValues trucksList

filterBySleeperBunk sleeperBunkFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.sleeperBunk sleeperBunkFilters) trucksList
                |> returnListWithValues trucksList

filterByEngineMake engineMakeFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.engineMake engineMakeFilters) trucksList
                |> returnListWithValues trucksList

filterByTransType transTypeFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.transType transTypeFilters) trucksList
                |> returnListWithValues trucksList

filterBySuspension suspensionFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.suspension suspensionFilters) trucksList
                |> returnListWithValues trucksList

filterByBodyType bodyTypeFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.bodyType bodyTypeFilters) trucksList
                |> returnListWithValues trucksList

filterByFleetCode fleetCodeFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.fleetCode fleetCodeFilters) trucksList
                |> returnListWithValues trucksList

filterByTruckStatus truckStatusFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.truckStatus truckStatusFilters) trucksList
                |> returnListWithValues trucksList

filterBySpecialFinancing specialFinancingFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.specialFinancing specialFinancingFilters) trucksList
                |> returnListWithValues trucksList

filterByOwningBranch owningBranchFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.owningBranch owningBranchFilters) trucksList
                |> returnListWithValues trucksList

filterByLocationName locationNameFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.locationName locationNameFilters) trucksList
                |> returnListWithValues trucksList

filterByRearAxleType rearAxleFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.rearAxleType rearAxleFilters) trucksList
                |> returnListWithValues trucksList

filterByTruckType truckTypeFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.truckType truckTypeFilters) trucksList
                |> returnListWithValues trucksList

filterByAPU apuFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.apu apuFilters) trucksList
                |> returnListWithValues trucksList

filterByCDL cdlFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.cdl cdlFilters) trucksList
                |> returnListWithValues trucksList

filterByPhoto photoFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.hasPhoto photoFilters) trucksList
                |> returnListWithValues trucksList

----------Range filters

filterByPrice priceFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.price priceFilters) trucksList
                |> returnListWithValues trucksList

filterByEngineHP engineHPFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.engineHP engineHPFilters) trucksList
                |> returnListWithValues trucksList

filterBySleeperInches sleeperInchesFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.sleeperInches sleeperInchesFilters) trucksList
                |> returnListWithValues trucksList

filterByWheelBase wheelBaseFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.wheelBase wheelBaseFilters) trucksList
                |> returnListWithValues trucksList

filterByMileage mileageFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.mileage mileageFilters) trucksList
                |> returnListWithValues trucksList

filterByFrontAxleWeight frontAxleWeightFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.frontAxleWeight frontAxleWeightFilters) trucksList
                |> returnListWithValues trucksList

filterByRearAxleWeight rearAxleWeightFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.rearAxleWeight rearAxleWeightFilters) trucksList
                |> returnListWithValues trucksList

filterByInventoryAge inventoryAgeFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.inventoryAge inventoryAgeFilters) trucksList
                |> returnListWithValues trucksList

                        
buildTruckIdNumber : Truck -> (String, String)
buildTruckIdNumber truck =
    if truck.stockNumber > 0 then 
        ("Stock#: " , "i0" ++ String.fromInt truck.stockNumber)
    else if truck.appraisalNumber > 0 then 
        ("Appraisal#: " , "A" ++ String.fromInt truck.appraisalNumber)
    else
        ("PO#: " , "P" ++ truck.poNumber)
        
hasAnyOfSearchFilterValuesChecked searchFilters =
        searchFilters
        |> Array.filter (\sf -> sf.userAction == True) --user count func, todo
        |> Array.length
        |> (\length  -> length > 0)

-- the below two func shows the power of partial apps
hasThisTruckMatchesWithUserSelectedFilterValue filterList partialCompareWaitingForSecondParamSearchFilter = 
        filterList
                |> Array.filter partialCompareWaitingForSecondParamSearchFilter -- funny name :)
                |> Array.length
                |> (\length  -> length > 0)

buildFilteredSearchResultBySearchType filterList comparefilterKeyValueWithTruckParam trucks =
        if hasAnyOfSearchFilterValuesChecked filterList then
                List.filter (\truck -> 
                                        hasThisTruckMatchesWithUserSelectedFilterValue filterList (comparefilterKeyValueWithTruckParam truck)
                                )  trucks 
        else
                trucks

funcList uiModel= [
                        (SalesStatus, filterBySalesStatus uiModel.salesStatusFilters)
                        , (Year,filterByYear uiModel.yearFilters)
                        , (Make,filterByMake uiModel.makeFilters)
                        , (MakeModel,filterByModel uiModel.modelFilters)
                        , (SleeperRoof,filterBySleeperRoof uiModel.sleeperRoofFilters)
                        , (SleeperBunk,filterBySleeperBunk uiModel.sleeperBunkFilters)
                        , (EngineMake,filterByEngineMake uiModel.engineMakeFilters)
                        , (TransType,filterByTransType uiModel.transTypeFilters)
                        , (Suspension,filterBySuspension uiModel.suspensionFilters)
                        , (BodyType,filterByBodyType uiModel.bodyTypeFilters)
                        , (RearAxleType,filterByRearAxleType uiModel.rearAxleTypeFilters)
                        , (TruckType,filterByTruckType uiModel.truckTypeFilters)
                        , (FleetCode,filterByFleetCode uiModel.fleetCodeFilters)
                        , (SpecialFinancing,filterBySpecialFinancing uiModel.specialFinancingFilters)
                        , (OwningBranch,filterByOwningBranch uiModel.owningBranchFilters)
                        , (LocationName,filterByLocationName  uiModel.locationNameFilters)
                        , (APU,filterByAPU uiModel.apuFilters)
                        , (CDL,filterByCDL uiModel.cdlFilters)
                        , (Photo,filterByPhoto uiModel.photoFilters)
                                --range filters
                        , (Price,filterByPrice uiModel.priceFilters)
                        , (EngineHP,filterByEngineHP uiModel.engineHPFilters)
                        , (SleeperInches,filterBySleeperInches uiModel.sleeperInchesFilters)
                        , (WheelBase,filterByWheelBase uiModel.wheelBaseFilters)
                        , (Mileage,filterByMileage uiModel.mileageFilters)
                        , (FrontAxleWeight,filterByFrontAxleWeight uiModel.frontAxleWeightFilters)
                        , (RearAxleWeight,filterByRearAxleWeight uiModel.rearAxleWeightFilters)
                        , (InventoryAge,filterByInventoryAge uiModel.inventoryAgeFilters)
                ]

-- applyFilterList excludeFilterType trucks uiModel all =
--         let
--                 reducedList = List.map (\(typ, fn) ->                 
--                                                         let
--                                                                 filteredList = trucks
--                                                                                         |> fn
--                                                         in
--                                                                 List.map (\t -> t.name) filteredList
                                                                
--                                                                 -- if List.length filteredList == List.length model.truckList then
--                                                                 --         List.map (\t -> t.name) filteredList
--                                                                 -- else
--                                                                 --         List.map (\t -> t.name) filteredList
                                                        
--                                                 ) <|
--                                                         -- if all then
--                                                         --         funcList uiModel
                                                                
--                                                         -- else
--                                                                 List.filter (\(typ, fn) -> typ /= excludeFilterType) <| funcList uiModel

--                 --yu = List.sort <| List.concat <| reducedList

--                 yu = List.concat <| reducedList

--                 jkv = Debug.log "REdcue list " [ yu ]

--                 hh = List.filter(\stnum -> 
--                                 --let
--                                         --cnt = 
--                                         count (\x -> x == stnum ) yu    ==  26
--                                 --in
--                                   --      cnt == 26
--                         ) yu

--                 fr = filterDuplicates hh
--                 --v = Debug.log "REdcue list " [ filterDuplicates hh ]

--                 filteredTruckList = List.map (\stnum ->
--                                                         --let
--                                                                find (\t -> t.name == stnum ) trucks    
                                                               
                                                                
--                                                         --in
--                                                               -- case trk of
--                                                                 --        Just val -> val
--                                                                 --        Nothing -> Truck         
                                                        
--                                                 ) fr
    
--                 --vj = Debug.log "REdcue list " [ filteredTruckList ]

                
--         in
--                 --filterDuplicates hh 
--                 values filteredTruckList


rebuildSearchFiltersBasedOnCurrentSearchCriteria : Model -> UIModel -> UIModel
rebuildSearchFiltersBasedOnCurrentSearchCriteria model uiModel =
        let 
                findMatchAndSetUserAction filters sf =
                        filters
                                |> Array.filter(\uiSF -> uiSF.searchFilterKey == sf.searchFilterKey)
                                |> Array.toList
                                |> List.head
                                |> (\headItem -> 
                                        case headItem of 
                                                Just val -> val
                                                Nothing -> sf)
                                |> (\headItem -> {sf | userAction = headItem.userAction} )

                updatedSalesStatusFitlerList =
                         model.truckList
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters
                                
                                >> buildSearchFilterValueRecordList SalesStatus uiModel.salesStatusFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.salesStatusFilters sf 
                                        )

                updatedYearFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters 
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters
                                
                                >> buildSearchFilterValueRecordList Year uiModel.yearFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.yearFilters sf 
                                        )
                
                updatedMakeFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters
                                
                                >> buildSearchFilterValueRecordList Make uiModel.makeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.makeFilters sf 
                                        )
                
                updatedModelFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters 
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters
                                
                                >> buildSearchFilterValueRecordList MakeModel uiModel.modelFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.modelFilters sf 
                                        )

                updatedSleeperRoofFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters
                                
                                >> buildSearchFilterValueRecordList SleeperRoof uiModel.sleeperRoofFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.sleeperRoofFilters sf 
                                        )

                updatedSleeperBunkFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters
                                
                                >> buildSearchFilterValueRecordList SleeperBunk uiModel.sleeperBunkFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.sleeperBunkFilters sf 
                                        )

                updatedEngineMakeFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters 
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList EngineMake uiModel.engineMakeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.engineMakeFilters sf 
                                        )

                
                
                updatedTransTypeFitlerList =
                        model.truckList
                               |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters    
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList TransType uiModel.transTypeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.transTypeFilters sf 
                                        )
                
                updatedSuspensionFilterList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList Suspension uiModel.suspensionFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.suspensionFilters sf 
                                        )
                
                updatedBodyTypeFilterList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList BodyType uiModel.bodyTypeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.bodyTypeFilters sf 
                                        )

                updatedRearAxleFilterList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList RearAxleType uiModel.rearAxleTypeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.rearAxleTypeFilters sf 
                                        )

                updatedFleetCodeFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList FleetCode uiModel.fleetCodeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.fleetCodeFilters sf 
                                        )
                
                updatedSpecialFinancingFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList SpecialFinancing uiModel.specialFinancingFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.specialFinancingFilters sf 
                                        )
                                        
                updatedOwningBranchFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList OwningBranch uiModel.owningBranchFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.owningBranchFilters sf 
                                        )

                updatedLocationNameFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList LocationName uiModel.locationNameFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.locationNameFilters sf 
                                        )

                updatedAPUFilterList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList APU uiModel.apuFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.apuFilters sf 
                                        )

                updatedCDLFilterList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList CDL uiModel.cdlFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.cdlFilters sf 
                                        )

                updatedPhotoFilterList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList Photo uiModel.photoFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.photoFilters sf
                                        )                                        


-- range filters
                updatedPriceFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters
                                
                                >> buildSearchFilterValueRecordList Price uiModel.priceFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.priceFilters sf 
                                        )
                
                updatedEngineHPFitlerList =
                        model.truckList
                               |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList EngineHP uiModel.engineHPFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.engineHPFilters sf 
                                        )
                

                updatedSleeperInchesFilterList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList SleeperInches uiModel.sleeperInchesFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.sleeperInchesFilters sf 
                                        )
                
                updatedWheelBaseFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList WheelBase uiModel.wheelBaseFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.wheelBaseFilters sf 
                                        )
                
                updatedMileageFilterList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList Mileage uiModel.mileageFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.mileageFilters sf 
                                        )
                
                updatedFrontAxleWeightFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList FrontAxleWeight uiModel.frontAxleWeightFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.frontAxleWeightFilters sf 
                                        )
                
                updatedRearAxleWeightFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList RearAxleWeight uiModel.rearAxleWeightFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.rearAxleWeightFilters sf 
                                        )
                
                
                
                updatedInventoryAgeFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByTruckType uiModel.truckTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters

                                >> buildSearchFilterValueRecordList InventoryAge uiModel.inventoryAgeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.inventoryAgeFilters sf 
                                        )
                
                updatedTruckTypeFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineMake uiModel.engineMakeFilters
                                |> filterByTransType uiModel.transTypeFilters
                                |> filterBySuspension uiModel.suspensionFilters
                                |> filterByBodyType uiModel.bodyTypeFilters
                                |> filterByRearAxleType uiModel.rearAxleTypeFilters
                                |> filterByFleetCode uiModel.fleetCodeFilters
                                
                                |> filterBySpecialFinancing uiModel.specialFinancingFilters
                                |> filterByOwningBranch uiModel.owningBranchFilters
                                |> filterByLocationName  uiModel.locationNameFilters
                                |> filterByAPU uiModel.apuFilters
                                |> filterByCDL uiModel.cdlFilters
                                |> filterByPhoto uiModel.photoFilters
                                --range filters
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                |> filterBySleeperInches uiModel.sleeperInchesFilters
                                |> filterByWheelBase uiModel.wheelBaseFilters
                                |> filterByMileage uiModel.mileageFilters
                                |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                                |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                                |> filterByInventoryAge uiModel.inventoryAgeFilters

                                >> buildSearchFilterValueRecordList TruckType uiModel.truckTypeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.truckTypeFilters sf 
                                        )


                
                newUIModel = 
                        {
                                uiModel |
                                                yearFilters = updatedYearFitlerList
                                                , salesStatusFilters = updatedSalesStatusFitlerList
                                                , makeFilters = updatedMakeFitlerList
                                                , modelFilters = updatedModelFitlerList
                                                , sleeperRoofFilters = updatedSleeperRoofFitlerList
                                                , sleeperBunkFilters = updatedSleeperBunkFitlerList
                                                , engineMakeFilters = updatedEngineMakeFitlerList
                                                , transTypeFilters = updatedTransTypeFitlerList
                                                , suspensionFilters = updatedSuspensionFilterList
                                                , bodyTypeFilters = updatedBodyTypeFilterList
                                                , rearAxleTypeFilters = updatedRearAxleFilterList
                                                , priceFilters = updatedPriceFitlerList
                                                , engineHPFilters = updatedEngineHPFitlerList
                                                , sleeperInchesFilters = updatedSleeperInchesFilterList
                                                , wheelBaseFilters = updatedWheelBaseFitlerList
                                                , mileageFilters = updatedMileageFilterList
                                                , frontAxleWeightFilters = updatedFrontAxleWeightFitlerList
                                                , rearAxleWeightFilters = updatedRearAxleWeightFitlerList
                                                , truckTypeFilters = updatedTruckTypeFitlerList
                                                , fleetCodeFilters = updatedFleetCodeFitlerList
                                                , specialFinancingFilters = updatedSpecialFinancingFitlerList
                                                , inventoryAgeFilters = updatedInventoryAgeFitlerList
                                                , owningBranchFilters = updatedOwningBranchFitlerList
                                                , locationNameFilters = updatedLocationNameFitlerList
                                                , apuFilters = updatedAPUFilterList
                                                , cdlFilters = updatedCDLFilterList
                                                , photoFilters = updatedPhotoFilterList
                        }
        in
                newUIModel

applySearchFilters: Model -> UIModel -> List Truck
applySearchFilters model uiModel =
    let
        
        filterdTruckList  = 
                model.truckList 
                        -- truckList gets passed as a last arg automatically from the previous |> pipe
                        -- the result from the above function gets feed in to the below function and so on until it ends
                        |> filterBySalesStatus uiModel.salesStatusFilters
                        |> filterByYear uiModel.yearFilters
                        |> filterByMake uiModel.makeFilters
                        |> filterByModel uiModel.modelFilters
                        |> filterBySleeperRoof uiModel.sleeperRoofFilters
                        |> filterBySleeperBunk uiModel.sleeperBunkFilters
                        |> filterByEngineMake uiModel.engineMakeFilters
                        |> filterByTransType uiModel.transTypeFilters  
                        |> filterBySuspension uiModel.suspensionFilters
                        |> filterByBodyType uiModel.bodyTypeFilters
                        |> filterByRearAxleType uiModel.rearAxleTypeFilters
                        |> filterByTruckType uiModel.truckTypeFilters  
                        |> filterByFleetCode uiModel.fleetCodeFilters
                        
                        |> filterBySpecialFinancing uiModel.specialFinancingFilters
                        |> filterByOwningBranch uiModel.owningBranchFilters
                        |> filterByLocationName  uiModel.locationNameFilters
                        |> filterByAPU uiModel.apuFilters
                        |> filterByCDL uiModel.cdlFilters
                        |> filterByPhoto uiModel.photoFilters
                        -- range filters
                        |> filterByPrice uiModel.priceFilters
                        |> filterByEngineHP uiModel.engineHPFilters                      
                        |> filterBySleeperInches uiModel.sleeperInchesFilters
                        |> filterByWheelBase uiModel.wheelBaseFilters
                        |> filterByMileage uiModel.mileageFilters
                        |> filterByFrontAxleWeight uiModel.frontAxleWeightFilters
                        |> filterByRearAxleWeight uiModel.rearAxleWeightFilters
                        |> filterByInventoryAge uiModel.inventoryAgeFilters

        sortedFilterdTruckList =
            filterdTruckList

    in
        sortedFilterdTruckList

sortTruckList sortBy listToSort =
                    case sortBy of 
                        PriceLowToHigh ->
                            listToSort
                                |> List.sortBy .price 
                        PriceHighToLow ->
                            listToSort
                                |> List.sortWith desendingOrderByPrice
                        MileageLowToHigh ->
                            listToSort
                                |> List.sortBy .mileage 
                        MileageHighToLow ->
                            listToSort
                                |> List.sortWith desendingOrderByMileage
                        MakeAtoZ ->
                            listToSort
                                |> List.sortBy .make     
                        MakeZtoA ->
                            listToSort
                                |> List.sortWith desendingOrderByMake
                        YearOldToNew ->
                            listToSort
                                |> List.sortBy .year     
                        YearNewToOld ->
                            listToSort
                                |> List.sortWith desendingOrderByYear
                                
defaultSortBy  =
    MakeAtoZ

defaultSortByText  =
    "Make A to Z"

sortByItemslist : List (String, String, SortBy)
sortByItemslist = 
    [
        ("PriceLowToHigh","Price - Low to High",PriceLowToHigh),
        ("PriceHighToLow","Price - High to Low",PriceHighToLow),
        ("MileageLowToHigh","Mileage - Low to High",MileageLowToHigh),
        ("MileageHighToLow","Mileage - High to Low",MileageHighToLow),
        ("MakeAtoZ","Make A to Z",MakeAtoZ),
        ("MakeZtoA","Make Z to A",MakeZtoA),
        ("YearNewToOld","Year - New to Old",YearNewToOld),
        ("YearOldToNew","Year - Old to New",YearOldToNew)
    ]

convertSortByToDescription sortBy =
    sortByItemslist
        |> List.filter(\(_,_, v) -> v == sortBy)
        |> List.head
        |> Maybe.map (\(k, d, v) -> d)
        |> Maybe.withDefault defaultSortByText
                
convertSortByToKey sortBy =
    sortByItemslist
        |> List.filter(\(_,_, v) -> v == sortBy)
        |> List.head
        |> Maybe.map (\(k, d, v) -> k)
        |> Maybe.withDefault defaultSortByText

desendingOrderByPrice a b =
    case compare a.price b.price of
        LT -> GT
        EQ -> EQ
        GT -> LT

desendingOrderByMileage a b =
    case compare a.mileage b.mileage of
        LT -> GT
        EQ -> EQ
        GT -> LT

desendingOrderByMake a b =
    case compare a.make b.make of
        LT -> GT
        EQ -> EQ
        GT -> LT

desendingOrderByYear a b =
    case compare a.year b.year of
        LT -> GT
        EQ -> EQ
        GT -> LT
