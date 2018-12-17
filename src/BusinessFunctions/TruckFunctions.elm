module BusinessFunctions.TruckFunctions exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Model exposing (..)
import Array exposing (..)
import SearchFilterViews.SearchFilter exposing (..)

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

buildSearchFilter uniqueFilterValuesFromTextSearchResult getCountFunc filterCategory =
        Array.filter(\sf -> not <| String.isEmpty sf.searchFilterKey)
                << Array.indexedMap (\index filterValue -> 
                                                SearchFilterType   
                                                                index 
                                                                filterValue 
                                                                filterValue  
                                                                False 
                                                                (getCountFunc filterValue)
                                                                filterCategory
                        ) << Array.fromList <| uniqueFilterValuesFromTextSearchResult

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
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> (buildFilteredSearchResultBySearchType uiModel.engineHPFilters)
                                         (\t sf ->
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.engineHP >= minValue && t.engineHP <= maxValue && sf.userAction == True 
                                        )
                                -- >> (\trks -> -- this is just to log intermediate result from with the function chains
                                --         let
                                --                y = Debug.log "sales filters - >" [trks]
                                --         in
                                --                 trks
                                -- )
                                >> buildSearchFilterValueRecordList SalesStatus uiModel.salesStatusFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.salesStatusFilters sf 
                                        )

                updatedYearFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> (buildFilteredSearchResultBySearchType uiModel.engineHPFilters)
                                         (\t sf ->
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.engineHP >= minValue && t.engineHP <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList Year uiModel.yearFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.yearFilters sf 
                                        )
                
                updatedMakeFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> (buildFilteredSearchResultBySearchType uiModel.engineHPFilters)
                                         (\t sf ->
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.engineHP >= minValue && t.engineHP <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList Make uiModel.makeFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.makeFilters sf 
                                        )
                
                updatedModelFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> (buildFilteredSearchResultBySearchType uiModel.engineHPFilters)
                                         (\t sf ->
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.engineHP >= minValue && t.engineHP <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList MakeModel uiModel.modelFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.modelFilters sf 
                                        )

                updatedSleeperRoofFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> (buildFilteredSearchResultBySearchType uiModel.engineHPFilters)
                                         (\t sf ->
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.engineHP >= minValue && t.engineHP <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList SleeperRoof uiModel.sleeperRoofFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.sleeperRoofFilters sf 
                                        )

                updatedSleeperBunkFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> (buildFilteredSearchResultBySearchType uiModel.engineHPFilters)
                                         (\t sf ->
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.engineHP >= minValue && t.engineHP <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList SleeperBunk uiModel.sleeperBunkFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.sleeperBunkFilters sf 
                                        )
                
                updatedPriceFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.engineHPFilters)
                                         (\t sf ->
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.engineHP >= minValue && t.engineHP <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList Price uiModel.priceFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.priceFilters sf 
                                        )
                
                updatedEngineHPFitlerList =
                        model.truckList
                                |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True )
                                |> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                        (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                                >> (buildFilteredSearchResultBySearchType uiModel.priceFilters)
                                        (\t sf -> 
                                                let
                                                        minmaxValue = getMinMaxValue sf    
                                                        minValue = Tuple.first minmaxValue
                                                        maxValue = Tuple.second minmaxValue
                                                in
                                                        t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                        )
                                >> buildSearchFilterValueRecordList EngineHP uiModel.engineHPFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.engineHPFilters sf 
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
                                                , priceFilters = updatedPriceFitlerList
                                                , engineHPFilters = updatedEngineHPFitlerList
                        }
        in
                newUIModel

applySearchFilters: Model -> UIModel -> List Truck
applySearchFilters model uiModel =
    let
        filterdTruckList  = 
                model.truckList 
                        |> (buildFilteredSearchResultBySearchType uiModel.salesStatusFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.salesStatus && sf.userAction == True ) -- truckList gets passed as a last arg automatically from the previous |> pipe
                                -- the result from the above function gets feed in to the below function and so on until it ends
                        >> (buildFilteredSearchResultBySearchType uiModel.yearFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.year && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.makeFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.make && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.modelFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.model && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.sleeperRoofFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperRoof && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.sleeperBunkFilters)
                                (\t sf -> String.trim sf.searchFilterKey == String.trim t.sleeperBunk && sf.userAction == True )
                        >> (buildFilteredSearchResultBySearchType uiModel.priceFilters) 
                                (\t sf -> 
                                        let
                                                minmaxValue = getMinMaxValue sf    
                                                minValue = Tuple.first minmaxValue
                                                maxValue = Tuple.second minmaxValue
                                        in
                                                t.price >= minValue && t.price <= maxValue && sf.userAction == True 
                                )
                        >> (buildFilteredSearchResultBySearchType uiModel.engineHPFilters)
                                (\t sf -> 
                                        let
                                                minmaxValue = getMinMaxValue sf    
                                                minValue = Tuple.first minmaxValue
                                                maxValue = Tuple.second minmaxValue
                                        in
                                                t.engineHP >= minValue && t.engineHP <= maxValue && sf.userAction == True 
                                )

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

-- convertStringToSortBy : String -> SortBy
-- convertStringToSortBy key = 
--     sortByItemslist
--         |> List.filter (\(k, d, v) -> k == key)
--         |> List.head
--         |> Maybe.map (\(k, d, v) -> v)
--         |> Maybe.withDefault defaultSortBy

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

--flippedComparison a b =
desendingOrderByPrice a b =
    case compare a.price b.price of
        LT -> GT
        EQ -> EQ
        GT -> LT

--flippedComparison a b =
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