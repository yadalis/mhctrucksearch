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

anyFilterApplied uiModel =
        (selectedFiltersCount <| Array.fromList <| concatAllFilters uiModel)
                |> (\res -> res > 0)

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
                                --         let
                                --                 trimmedsfValue = String.trim sf.searchFilterKey
                                --                 v = Debug.log "asdfdddddddddddddddddddd" trimmedsfValue
                                --                 displayValue = 
                                --                                                 if trimmedsfValue == "I" then
                                --                                                         "Inventory"
                                --                                                 else if trimmedsfValue == "A" then 
                                --                                                         "Appraisal"
                                --                                                 else
                                --                                                         "Purchase Order"
                                --         in
                                --                 displayValue
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

filterBySalesStatus (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.salesStatus (getSelectedFilterBulletsByFilterCategory SalesStatus selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByYear (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.year (getSelectedFilterBulletsByFilterCategory Year selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByMake (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.make (getSelectedFilterBulletsByFilterCategory Make selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))                

filterByModel (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.model (getSelectedFilterBulletsByFilterCategory MakeModel selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySleeperRoof (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.sleeperRoof (getSelectedFilterBulletsByFilterCategory SleeperRoof selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySleeperBunk (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.sleeperBunk (getSelectedFilterBulletsByFilterCategory SleeperBunk selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByEngineMake (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.engineMake (getSelectedFilterBulletsByFilterCategory EngineMake selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByTransType (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.transType (getSelectedFilterBulletsByFilterCategory TransType selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySuspension (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.suspension (getSelectedFilterBulletsByFilterCategory Suspension selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByBodyType (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.bodyType (getSelectedFilterBulletsByFilterCategory BodyType selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByFleetCode (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.fleetCode (getSelectedFilterBulletsByFilterCategory FleetCode selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySpecialFinancing (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.specialFinancing (getSelectedFilterBulletsByFilterCategory SpecialFinancing selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByOwningBranch (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.owningBranch (getSelectedFilterBulletsByFilterCategory OwningBranch selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByLocationName (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.locationName (getSelectedFilterBulletsByFilterCategory LocationName selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByRearAxleType (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.rearAxleType (getSelectedFilterBulletsByFilterCategory RearAxleType selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByTruckType (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.truckType (getSelectedFilterBulletsByFilterCategory TruckType selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByAPU (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.apu (getSelectedFilterBulletsByFilterCategory APU selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByCDL (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.cdl (getSelectedFilterBulletsByFilterCategory CDL selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByPhoto (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedFilters t.hasPhoto (getSelectedFilterBulletsByFilterCategory Photo selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

----------Range filters

filterByPrice (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.price (getSelectedFilterBulletsByFilterCategory Price selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByEngineHP (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.engineHP (getSelectedFilterBulletsByFilterCategory EngineHP selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterBySleeperInches (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.sleeperInches (getSelectedFilterBulletsByFilterCategory SleeperInches selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByWheelBase (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.wheelBase (getSelectedFilterBulletsByFilterCategory WheelBase selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByMileage (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.mileage (getSelectedFilterBulletsByFilterCategory Mileage selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByFrontAxleWeight (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.frontAxleWeight (getSelectedFilterBulletsByFilterCategory FrontAxleWeight selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByRearAxleWeight (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.rearAxleWeight (getSelectedFilterBulletsByFilterCategory RearAxleWeight selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))

filterByInventoryAge (selectedFilters, trucksList) =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.inventoryAge (getSelectedFilterBulletsByFilterCategory InventoryAge selectedFilters)) trucksList
                |> returnListWithValues trucksList
                |> (\trks -> (selectedFilters, trks))
                        
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

-- funcList uiModel= [
--                         (SalesStatus, filterBySalesStatus uiModel.salesStatusFilters)
--                         , (Year,filterByYear uiModel.yearFilters)
--                         , (Make,filterByMake uiModel.makeFilters)
--                         , (MakeModel,filterByModel uiModel.modelFilters)
--                         , (SleeperRoof,filterBySleeperRoof uiModel.sleeperRoofFilters)
--                         , (SleeperBunk,filterBySleeperBunk uiModel.sleeperBunkFilters)
--                         , (EngineMake,filterByEngineMake uiModel.engineMakeFilters)
--                         , (TransType,filterByTransType uiModel.transTypeFilters)
--                         , (Suspension,filterBySuspension uiModel.suspensionFilters)
--                         , (BodyType,filterByBodyType uiModel.bodyTypeFilters)
--                         , (RearAxleType,filterByRearAxleType uiModel.rearAxleTypeFilters)
--                         , (TruckType,filterByTruckType uiModel.truckTypeFilters)
--                         , (FleetCode,filterByFleetCode uiModel.fleetCodeFilters)
--                         , (SpecialFinancing,filterBySpecialFinancing uiModel.specialFinancingFilters)
--                         , (OwningBranch,filterByOwningBranch uiModel.owningBranchFilters)
--                         , (LocationName,filterByLocationName  uiModel.locationNameFilters)
--                         , (APU,filterByAPU uiModel.apuFilters)
--                         , (CDL,filterByCDL uiModel.cdlFilters)
--                         , (Photo,filterByPhoto uiModel.photoFilters)
--                                 --range filters
--                         , (Price,filterByPrice uiModel.priceFilters)
--                         , (EngineHP,filterByEngineHP uiModel.engineHPFilters)
--                         , (SleeperInches,filterBySleeperInches uiModel.sleeperInchesFilters)
--                         , (WheelBase,filterByWheelBase uiModel.wheelBaseFilters)
--                         , (Mileage,filterByMileage uiModel.mileageFilters)
--                         , (FrontAxleWeight,filterByFrontAxleWeight uiModel.frontAxleWeightFilters)
--                         , (RearAxleWeight,filterByRearAxleWeight uiModel.rearAxleWeightFilters)
--                         , (InventoryAge,filterByInventoryAge uiModel.inventoryAgeFilters)
--                 ]

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

getSelectedFilterBulletsByFilterCategory filterCategory selectedFilterList =
        (Array.fromList <| List.filter (\sf -> sf.filterCategory == filterCategory)  selectedFilterList)

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

                rebuildFilters filterCategory filters (selectedFilterBullets, finalFilteredTrucks) =
                        buildSearchFilterValueRecordList filterCategory filters finalFilteredTrucks     
                        |> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction (Array.fromList selectedFilterBullets) sf 
                                        )

                updatedSalesStatusFitlerList =
                         (uiModel.selectedFilterBullets, model.truckList)
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName 
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters SalesStatus uiModel.salesStatusFilters

                updatedTruckTypeFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters  TruckType uiModel.truckTypeFilters

                updatedYearFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch 
                                |> filterByLocationName 
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters Year uiModel.yearFilters
                
                updatedMakeFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters Make uiModel.makeFilters
                 
                updatedModelFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch 
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters MakeModel uiModel.modelFilters

                updatedSleeperRoofFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters SleeperRoof uiModel.sleeperRoofFilters
 
                updatedSleeperBunkFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters SleeperBunk uiModel.sleeperBunkFilters

                updatedEngineMakeFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch 
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters EngineMake uiModel.engineMakeFilters
                
                updatedTransTypeFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch    
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters TransType uiModel.transTypeFilters
                
                updatedSuspensionFilterList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters Suspension uiModel.suspensionFilters
                 
                updatedBodyTypeFilterList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters BodyType uiModel.bodyTypeFilters

                updatedRearAxleFilterList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters RearAxleType uiModel.rearAxleTypeFilters

                updatedFleetCodeFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters FleetCode uiModel.fleetCodeFilters

                updatedSpecialFinancingFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters SpecialFinancing uiModel.specialFinancingFilters

                updatedOwningBranchFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterByLocationName
                                |> filterBySpecialFinancing
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters OwningBranch uiModel.owningBranchFilters
                                
                updatedLocationNameFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterByOwningBranch
                                |> filterBySpecialFinancing
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters LocationName uiModel.locationNameFilters

                updatedAPUFilterList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterByLocationName
                                |> filterByOwningBranch
                                |> filterBySpecialFinancing
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters APU uiModel.apuFilters

                updatedCDLFilterList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterByLocationName
                                |> filterByOwningBranch
                                |> filterBySpecialFinancing
                                |> filterByAPU
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters CDL uiModel.cdlFilters

                updatedPhotoFilterList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterByLocationName
                                |> filterByOwningBranch
                                |> filterBySpecialFinancing
                                |> filterByAPU
                                |> filterByCDL
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters Photo uiModel.photoFilters
                                
-- range filters
                updatedPriceFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters Price uiModel.priceFilters

                updatedEngineHPFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters EngineHP uiModel.engineHPFilters

                updatedSleeperInchesFilterList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters SleeperInches uiModel.sleeperInchesFilters

                updatedWheelBaseFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters WheelBase uiModel.wheelBaseFilters

                updatedMileageFilterList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters Mileage uiModel.mileageFilters

                updatedFrontAxleWeightFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByRearAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters FrontAxleWeight uiModel.frontAxleWeightFilters

                updatedRearAxleWeightFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByInventoryAge
                                |> rebuildFilters RearAxleWeight uiModel.rearAxleWeightFilters

                updatedInventoryAgeFitlerList =
                        (uiModel.selectedFilterBullets, model.truckList)
                                |> filterBySalesStatus
                                |> filterByYear
                                |> filterByMake
                                |> filterByModel
                                |> filterBySleeperRoof
                                |> filterBySleeperBunk
                                |> filterByEngineMake
                                |> filterByTransType
                                |> filterBySuspension
                                |> filterByBodyType
                                |> filterByRearAxleType
                                |> filterByTruckType
                                |> filterByFleetCode
                                |> filterBySpecialFinancing
                                |> filterByOwningBranch
                                |> filterByLocationName
                                |> filterByAPU
                                |> filterByCDL
                                |> filterByPhoto
                                --range filters
                                |> filterByPrice
                                |> filterByEngineHP
                                |> filterBySleeperInches
                                |> filterByWheelBase
                                |> filterByMileage
                                |> filterByFrontAxleWeight
                                |> filterByRearAxleWeight
                                |> rebuildFilters InventoryAge uiModel.inventoryAgeFilters
                
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
                 (uiModel.selectedFilterBullets, model.truckList) 
                        |> filterBySalesStatus 
                        |> filterByYear 
                        |> filterByMake 
                        |> filterByModel 
                        |> filterBySleeperRoof
                        |> filterBySleeperBunk
                        |> filterByEngineMake
                        |> filterByTransType  
                        |> filterBySuspension
                        |> filterByBodyType
                        |> filterByRearAxleType
                        |> filterByTruckType  
                        |> filterByFleetCode
                        |> filterBySpecialFinancing 
                        |> filterByOwningBranch 
                        |> filterByLocationName
                        |> filterByAPU
                        |> filterByCDL
                        |> filterByPhoto
                        -- range filters
                        |> filterByPrice
                        |> filterByEngineHP                      
                        |> filterBySleeperInches
                        |> filterByWheelBase
                        |> filterByMileage
                        |> filterByFrontAxleWeight
                        |> filterByRearAxleWeight
                        |> filterByInventoryAge
                        |> \(_, finalFilteredTrucks) -> finalFilteredTrucks

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
