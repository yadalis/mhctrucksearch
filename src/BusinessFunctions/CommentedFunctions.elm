-- module BusinessFunctions.SearchFilterFunctions exposing (..)

-- import Model exposing (..)
-- import Array exposing (..)
-- import SearchFilterViews.SearchFilter exposing (..)
-- import List.Extra exposing (..)
-- import List.Unique exposing (..)
-- import Maybe.Extra exposing (..)
 
-- resetFilters filters = 
--         Array.map(\sf -> {sf | userAction = False } ) filters
--         -- if (Array.length filters) > 0 then
--         --         Array.map(\sf -> {sf | userAction = False } ) filters
--         -- else
--         --         filters

-- anyFilterApplied uiModel =
--         List.length (uiModel.selectedFilterBullets) > 0


-- getSelectedFilterBulletsByFilterCategory filterCategory selectedFilterList =
--         (Array.fromList <| List.filter (\sf -> sf.filterCategory == filterCategory)  selectedFilterList)
        
-- selectedFiltersCount filters = 
--         count (\sf -> sf.userAction == True) <| Array.toList filters
--         --Array.length <| Array.filter (\sf -> sf.userAction == True) filters

-- anyFilterApplied uiModel =
--         (selectedFiltersCount <| Array.fromList <| concatAllFilters uiModel)
--                 |> (\res -> res > 0)

-- concatAllFilters uiModel =
--         List.concat
--         [ 
--                 Array.toList uiModel.salesStatusFilters,
--                 Array.toList uiModel.yearFilters,
--                 Array.toList uiModel.makeFilters,
--                 Array.toList uiModel.modelFilters,
--                 Array.toList uiModel.sleeperRoofFilters,
--                 Array.toList uiModel.sleeperBunkFilters,
--                 Array.toList uiModel.engineMakeFilters,
--                 Array.toList uiModel.transTypeFilters,
--                 Array.toList uiModel.suspensionFilters,
--                 Array.toList uiModel.bodyTypeFilters,
--                 Array.toList uiModel.rearAxleTypeFilters,
--                 Array.toList uiModel.truckTypeFilters,
--                 Array.toList uiModel.fleetCodeFilters,
--                 Array.toList uiModel.specialFinancingFilters,
--                 Array.toList uiModel.owningBranchFilters,
--                 Array.toList uiModel.apuFilters,
--                 Array.toList uiModel.cdlFilters,
--                 Array.toList uiModel.photoFilters,
--                 Array.toList uiModel.priceFilters,
--                 Array.toList uiModel.engineHPFilters,
--                 Array.toList uiModel.sleeperInchesFilters,
--                 Array.toList uiModel.wheelBaseFilters,
--                 Array.toList uiModel.mileageFilters,
--                 Array.toList uiModel.frontAxleWeightFilters,
--                 Array.toList uiModel.rearAxleWeightFilters,
--                 Array.toList uiModel.inventoryAgeFilters,
--                 Array.toList uiModel.locationNameFilters
--         ]
 
 
-- hasAnyOfSearchFilterValuesChecked searchFilters =
--         searchFilters
--                 |> Array.filter (\sf -> sf.userAction == True) --user count func, todo
--                 |> Array.length
--                 |> (\length  -> length > 0)

-- -- the below two func shows the power of partial apps
-- hasThisTruckMatchesWithUserSelectedFilterValue filterList partialCompareWaitingForSecondParamSearchFilter = 
--         filterList
--                 |> Array.filter partialCompareWaitingForSecondParamSearchFilter -- funny name :)
--                 |> Array.length
--                 |> (\length  -> length > 0)

-- buildFilteredSearchResultBySearchType filterList comparefilterKeyValueWithTruckParam trucks =
--         if hasAnyOfSearchFilterValuesChecked filterList then
--                 List.filter (\truck -> 
--                                         hasThisTruckMatchesWithUserSelectedFilterValue filterList (comparefilterKeyValueWithTruckParam truck)
--                                 )  trucks 
--         else
--                 trucks

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



                --  (uiModel.selectedFilterBullets, model.truckList) 
                --         |> filterBySalesStatus 
                --         |> filterByYear 
                --         |> filterByMake 
                --         |> filterByModel 
                --         |> filterBySleeperRoof
                --         |> filterBySleeperBunk
                --         |> filterByEngineMake
                --         |> filterByTransType  
                --         |> filterBySuspension
                --         |> filterByBodyType
                --         |> filterByRearAxleType
                --         |> filterByTruckType  
                --         |> filterByFleetCode
                --         |> filterBySpecialFinancing 
                --         |> filterByOwningBranch 
                --         |> filterByLocationName
                --         |> filterByAPU
                --         |> filterByCDL
                --         |> filterByPhoto
                --         -- range filters
                --         |> filterByPrice
                --         |> filterByEngineHP                      
                --         |> filterBySleeperInches
                --         |> filterByWheelBase
                --         |> filterByMileage
                --         |> filterByFrontAxleWeight
                --         |> filterByRearAxleWeight
                --         |> filterByInventoryAge
                        --|> \(_, finalFilteredTrucks) -> finalFilteredTrucks

-- resetFilters filters = 
--         Array.map(\sf -> {sf | userAction = False } ) filters
--         -- if (Array.length filters) > 0 then
--         --         Array.map(\sf -> {sf | userAction = False } ) filters
--         -- else
--         --         filters

-- getSelectedSearchFilterKeys searchFilters =
--     searchFilters 
--         |> Array.map (\sf -> sf.searchFilterKey) 
--         |> Array.toList
--         -- searchFilters 
--         --         |> Array.filter (\sf -> sf.userAction == True)
--         --         |> Array.map (\sf -> sf.searchFilterKey) 
--         --         |> Array.toList

-- getSelectedSearchFilterExtraData searchFilters =
--     searchFilters 
--         |> Array.map (\sf -> sf.searchFilterExtraData) 
--         |> Array.toList

--         -- searchFilters 
--         --         |> Array.filter (\sf -> sf.userAction == True)
--         --         |> Array.map (\sf -> sf.searchFilterExtraData) 
--         --         |> Array.toList

-- isGivenValueMatchesWithSelectedFilters value searchFilters  = 
--         --not <| notMember (String.trim value) <| getSelectedSearchFilterKeys searchFilters
--         getSelectedSearchFilterKeys searchFilters
--             |> notMember (String.trim value)
--             |> not