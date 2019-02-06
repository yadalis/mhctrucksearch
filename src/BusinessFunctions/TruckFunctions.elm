module BusinessFunctions.TruckFunctions exposing (..)

import Model exposing (..) 
                        
buildTruckIdNumber : Truck -> (String, String)
buildTruckIdNumber truck =
    if truck.stockNumber > 0 then 
        ("Stock#: " , "i0" ++ String.fromInt truck.stockNumber)
    else if truck.appraisalNumber > 0 then 
        ("Appraisal#: " , "A" ++ String.fromInt truck.appraisalNumber)
    else
        ("PO#: " , "P" ++ truck.poNumber)

-- sortTruckList sortBy listToSort =
--                     case sortBy of 
--                         PriceLowToHigh ->
--                             listToSort
--                                 |> List.sortBy .price 
--                         PriceHighToLow ->
--                             listToSort
--                                 |> List.sortWith desendingOrderByPrice
--                         MileageLowToHigh ->
--                             listToSort
--                                 |> List.sortBy .mileage 
--                         MileageHighToLow ->
--                             listToSort
--                                 |> List.sortWith desendingOrderByMileage
--                         MakeAtoZ ->
--                             listToSort
--                                 |> List.sortBy .make     
--                         MakeZtoA ->
--                             listToSort
--                                 |> List.sortWith desendingOrderByMake
--                         YearOldToNew ->
--                             listToSort
--                                 |> List.sortBy .year     
--                         YearNewToOld ->
--                             listToSort
--                                 |> List.sortWith desendingOrderByYear
                                
defaultSortBy  =
    MakeAtoZ

defaultSortByText  =
    "Make A to Z"

defaultSortByKey  =
    "MakeAtoZ"

sortByItemslist : List SortByMetaData
sortByItemslist = 
    [
        {sortByTextKey = "PriceLowToHigh", sortByText = "Price - Low to High", sortByField = PriceLowToHigh, sortOrder = SortASC},
        {sortByTextKey = "PriceHighToLow", sortByText = "Price - High to Low", sortByField = PriceHighToLow, sortOrder = SortDSC},
        {sortByTextKey = "MileageLowToHigh", sortByText = "Mileage - Low to High", sortByField = MileageLowToHigh, sortOrder = SortASC},
        {sortByTextKey = "MileageHighToLow", sortByText = "Mileage - Low to High", sortByField = MileageHighToLow, sortOrder = SortDSC},
        {sortByTextKey = "MakeAtoZ", sortByText = "Make - Low to High", sortByField = MakeAtoZ, sortOrder = SortASC},
        {sortByTextKey = "MakeZtoA", sortByText = "Make - Low to High", sortByField = MakeZtoA, sortOrder = SortDSC},
        {sortByTextKey = "YearOldToNew", sortByText = "Year - Old to New", sortByField = YearOldToNew, sortOrder = SortASC},
        {sortByTextKey = "YearNewToOld", sortByText = "Year - New to Old", sortByField = YearNewToOld, sortOrder = SortDSC}
    ]

convertSortByToDescription sortBy =
    sortByItemslist
        |> List.filter(\{sortByTextKey,sortByText,sortByField,sortOrder} -> sortByField == sortBy)
        |> List.head
        |> Maybe.map (\{sortByTextKey,sortByText,sortByField,sortOrder} -> sortByText)
        |> Maybe.withDefault defaultSortByText
                
convertSortByToKey sortBy =
    sortByItemslist
        |> List.filter(\{sortByTextKey,sortByText,sortByField,sortOrder} -> sortByField == sortBy)
        |> List.head
        |> Maybe.map (\{sortByTextKey,sortByText,sortByField,sortOrder} -> sortByTextKey)
        |> Maybe.withDefault defaultSortByKey

-- convertSortByToDescription sortBy =
--     sortByItemslist
--         |> List.filter(\(_,_, v) -> v == sortBy)
--         |> List.head
--         |> Maybe.map (\(k, d, v) -> d)
--         |> Maybe.withDefault defaultSortByText
                
-- convertSortByToKey sortBy =
--     sortByItemslist
--         |> List.filter(\(_,_, v) -> v == sortBy)
--         |> List.head
--         |> Maybe.map (\(k, d, v) -> k)
--         |> Maybe.withDefault defaultSortByKey

-- desendingOrderByPrice a b =
--     case compare a.price b.price of
--         LT -> GT
--         EQ -> EQ
--         GT -> LT

-- desendingOrderByMileage a b =
--     case compare a.mileage b.mileage of
--         LT -> GT
--         EQ -> EQ
--         GT -> LT

-- desendingOrderByMake a b =
--     case compare a.make b.make of
--         LT -> GT
--         EQ -> EQ
--         GT -> LT

-- desendingOrderByYear a b =
--     case compare a.year b.year of
--         LT -> GT
--         EQ -> EQ
--         GT -> LT
