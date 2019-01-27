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

sortByItemslist : List SortMetaData
sortByItemslist = 
    [
        {sortItemDisplayText = "Price - Low to High"     , sortBy = "Price"    , sortByField = PriceLowToHigh      , sortOrder = "ASC"},
        {sortItemDisplayText = "Price - High to Low"     , sortBy = "Price"    , sortByField = PriceHighToLow      , sortOrder = "DESC"},
        {sortItemDisplayText = "Mileage - Low to High"   , sortBy = "Mileage"  , sortByField = MileageLowToHigh    , sortOrder = "ASC"},
        {sortItemDisplayText = "Mileage - High to Low"   , sortBy = "Mileage"  , sortByField = MileageHighToLow    , sortOrder = "DESC"},
        {sortItemDisplayText = "Make A to Z"             , sortBy = "Make"     , sortByField = MakeAtoZ            , sortOrder = "ASC"},
        {sortItemDisplayText = "Make Z to A"             , sortBy = "Make"     , sortByField = MakeZtoA            , sortOrder = "DESC"},
        {sortItemDisplayText = "Year - Old to New"       , sortBy = "Year"     , sortByField = YearOldToNew        , sortOrder = "ASC"},
        {sortItemDisplayText = "Year - New to Old"       , sortBy = "Year"     , sortByField = YearNewToOld        , sortOrder = "DESC"}
    ]
