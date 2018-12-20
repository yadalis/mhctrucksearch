module BusinessFunctions.TruckFunctions exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Model exposing (..)
import Array exposing (..)
import SearchFilterViews.SearchFilter exposing (..)
import List.Extra exposing (..)

getSelectedSearchFilterKeys searchFilters =
        searchFilters 
                |> Array.filter (\sf -> sf.userAction == True)
                |> Array.map (\sf -> sf.searchFilterKey) 
                |> Array.toList

getSelectedSearchFilterExtraData searchFilters =
        searchFilters 
                |> Array.filter (\sf -> sf.userAction == True)
                |> Array.map (\sf -> sf.searchFilterExtraData) 
                |> Array.toList

isGivenValueMatchesWithSelectedFilters value searchFilters  = 
        not <| notMember value <| getSelectedSearchFilterKeys searchFilters

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

filterByPrice priceFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.price priceFilters) trucksList
                |> returnListWithValues trucksList

filterByEngineHP engineHPFilters trucksList =
        List.filter (\t -> isGivenValueMatchesWithSelectedRangeFilters t.engineHP engineHPFilters) trucksList
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
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                
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
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                
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
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                
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
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                
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
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                
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
                                |> filterByPrice uiModel.priceFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                
                                >> buildSearchFilterValueRecordList SleeperBunk uiModel.sleeperBunkFilters
                                >> Array.map
                                        (\sf ->
                                                findMatchAndSetUserAction uiModel.sleeperBunkFilters sf 
                                        )
                
                updatedPriceFitlerList =
                        model.truckList
                                |> filterBySalesStatus uiModel.salesStatusFilters
                                |> filterByYear uiModel.yearFilters
                                |> filterByMake uiModel.makeFilters
                                |> filterByModel uiModel.modelFilters
                                |> filterBySleeperRoof uiModel.sleeperRoofFilters
                                |> filterBySleeperBunk uiModel.sleeperBunkFilters
                                |> filterByEngineHP uiModel.engineHPFilters
                                
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
                                |> filterByPrice uiModel.priceFilters

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
                        -- truckList gets passed as a last arg automatically from the previous |> pipe
                        -- the result from the above function gets feed in to the below function and so on until it ends
                        |> filterBySalesStatus uiModel.salesStatusFilters
                        |> filterByYear uiModel.yearFilters
                        |> filterByMake uiModel.makeFilters
                        |> filterByModel uiModel.modelFilters
                        |> filterBySleeperRoof uiModel.sleeperRoofFilters
                        |> filterBySleeperBunk uiModel.sleeperBunkFilters
                        |> filterByPrice uiModel.priceFilters
                        |> filterByEngineHP uiModel.engineHPFilters

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