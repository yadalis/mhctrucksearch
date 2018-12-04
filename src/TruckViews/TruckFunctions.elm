module TruckViews.TruckFunctions exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import Model exposing (..)
import Msg exposing (..)
import List.Unique exposing (..)
import Array exposing (..)

buildSearchFilters : Model -> UIModel -> UIModel
buildSearchFilters model uiModel =
    let
        x = 5

        hasThisTruckYearMatchesWithUserSelectedYear truck = 
            uiModel.yearFilters
                |> Array.toList
                |> List.filter (\yfModel -> Tuple.first yfModel == truck.year && Tuple.second yfModel == True) 
                |> List.length
                |> (\length  -> length > 0)
            -- List.filter (\x -> Tuple.first x == truck.year && Tuple.second x == True) (Array.toList <| newUIModel.yearFilters )
            --     |> List.length
            --     |> (\length  -> length > 0)
            --List.member truck.year ( List.map (\x -> Tuple.first) (Array.toList <|  newUIModel.yearFilters ) )
            --Array.get truck.year  ( Array.map (\x -> Tuple.first x) uiModel.yearFilters )

        yearByFilterdTruckList  = 
                model.filteredTruckList
                    |> List.filter (\t -> hasThisTruckYearMatchesWithUserSelectedYear t )

    in
        uiModel

applySearchFilters: Model -> UIModel -> List Truck
applySearchFilters model uiModel =
    let
        -- hasThisTruckYearMatchesWithUserSelectedYear truck = 
        --     uiModel.yearFilters
        --         |> Array.toList
        --         |> List.filter (\yfModel -> Tuple.first yfModel == truck.year && Tuple.second yfModel == True) 
        --         |> List.length
        --         |> (\length  -> length > 0)
        --     -- List.filter (\x -> Tuple.first x == truck.year && Tuple.second x == True) (Array.toList <| newUIModel.yearFilters )
        --     --     |> List.length
        --     --     |> (\length  -> length > 0)
        --     --List.member truck.year ( List.map (\x -> Tuple.first) (Array.toList <|  newUIModel.yearFilters ) )
        --     --Array.get truck.year  ( Array.map (\x -> Tuple.first x) uiModel.yearFilters )

        -- yearByFilterdTruckList  = 
        --         model.truckList
        --             |> List.filter (\t -> hasThisTruckYearMatchesWithUserSelectedYear t )

        -- tempFilteredTruckList = 
        --     if List.length yearByFilterdTruckList > 0 then
        --         yearByFilterdTruckList
        --     else
        --         model.truckList
        

----------------------------------------------- MAKE ---------------------------------------------------------


        hasThisTruckMakeMatchesWithUserSelectedMake truck = 
            uiModel.makeFilters
                |> Array.toList
                |> List.filter (\mkModel -> String.trim mkModel.searchFilterKey == String.trim truck.make && mkModel.userAction == True) 
                |> List.length
                |> (\length  -> length > 0)
            -- List.filter (\x -> Tuple.first x == truck.year && Tuple.second x == True) (Array.toList <| newUIModel.yearFilters )
            --     |> List.length
            --     |> (\length  -> length > 0)
            --List.member truck.year ( List.map (\x -> Tuple.first) (Array.toList <|  newUIModel.yearFilters ) )
            --Array.get truck.year  ( Array.map (\x -> Tuple.first x) uiModel.yearFilters )

        makeByFilterdTruckList  = 
                model.truckList
                    |> List.filter (\t -> hasThisTruckMakeMatchesWithUserSelectedMake t )

----------------------------------------------- MAKE ---------------------------------------------------------

        newFilteredTruckList = 
            if List.length makeByFilterdTruckList > 0 then
                makeByFilterdTruckList
            else
                model.truckList
        
        -- cdlByFilteredTruckList = 
        --         if uiModel.filterCDLNoSelected then
        --             List.filter (\t -> String.trim t.cdl == "No" ) newFilteredTruckList
        --         else
        --             if uiModel.filterCDLYesSelected then
        --                 List.filter (\t -> String.trim t.cdl == "Yes" ) newFilteredTruckList
        --             else
        --                 newFilteredTruckList
         
        sortedFilterdTruckList =
            newFilteredTruckList
                |> List.sortBy .make
        
        -- filteredTruckList  = 
        --     sortedFilterdTruckList
        --         |> List.length
        --         |> (\sortedFilterdTruckListLength -> if sortedFilterdTruckListLength > 0 then  sortedFilterdTruckList else originalTruckList) 
    in
        sortedFilterdTruckList --filteredTruckList

buildCDLValueList : List Truck -> List String
buildCDLValueList trucks =
    List.map (\t -> t.cdl) trucks
        --|> filterDuplicates

-- buildCDLCheckboxList : Int -> List Truck -> Element Msg
-- buildCDLCheckboxList trucks =
--     List.map buildCDLCheckboxList trucks

buildCDLValueGroups : Model -> UIModel -> Element Msg
buildCDLValueGroups model uiModel =
    let
        cdlList = buildCDLValueList model.filteredTruckList
        
        cdlNoList =     List.filter (\cdl -> String.trim cdl == "No") cdlList

        cdlYesList =    List.filter (\cdl -> String.trim cdl == "Yes") cdlList
    in
        row[spy 15, wf]
        [
            column[spy 10, wf,  bw one]
            [
                row[bw 0, hf, bwb 1, wf, pdb 3]
                [
                    paragraph [bw one, fal, wf][textValue <| "CDL"]
                ]
                ,column[spy 10, pdl 15][
                    row[bw two]
                    [
                        filterCheckBox  uiModel "CDLNo"
                        , textValue <| "No (" ++  (String.fromInt <| (List.length cdlNoList))  ++ ")"
                    ]
                    ,row[bw two]
                    [
                        filterCheckBox  uiModel "CDLYes"
                        , textValue <| "Yes (" ++  (String.fromInt <| (List.length cdlYesList))  ++ ")"
                    ]
                ]
            ]
        ]

--flippedComparison a b =
desendingOrder a b =
    case compare a b of
        LT -> GT
        EQ -> EQ
        GT -> LT
        
buildYearValueList : List Truck -> Array Int
buildYearValueList trucks =
    List.map (\t -> t.year) trucks
        |> filterDuplicates
        |> List.sortWith desendingOrder -- to do descending order
        |> Array.fromList

buildYearValueTupleList : List Truck -> Array (Int, Bool)
buildYearValueTupleList trucks =
    buildYearValueList trucks
        |> Array.map (\year -> (year, False))

buildYearValueGroups : Model -> UIModel -> Element Msg
buildYearValueGroups model uiModel = --currentFilteredTrucks =
    let
        yearFilters = uiModel.yearFilters

        buildYearCheckboxes :  Int -> (Int, Bool) -> Element Msg
        buildYearCheckboxes index (year, userAction) =
            let
                yearWiseCount =    List.filter (\t -> t.year == year) model.filteredTruckList --currentFilteredTrucks
            in
                row[bw two]
                [
                    checkbox [bw one, pdr 5 ] {
                        onChange = FilterYearCheckBoxClicked index year
                        ,icon = buildChkBoxImage
                        , label = labelRight [] (el [] <| textValue (String.fromInt year) )
                        --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
                        , checked = userAction
                    }
                    , textValue <| " (" ++  (String.fromInt <| (List.length yearWiseCount))  ++ ")"
                ]
                -- if List.length yearWiseCount > 0 then
                --     row[bw two]
                --     [
                --         checkbox [bw one, pdr 5 ] {
                --             onChange = FilterYearCheckBoxClicked index year
                --             ,icon = buildChkBoxImage
                --             , label = labelRight [] (el [] <| textValue (String.fromInt year) )
                --             --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
                --             , checked = userAction
                --         }
                --         , textValue <| " (" ++  (String.fromInt <| (List.length yearWiseCount))  ++ ")"
                --     ]
                -- else
                --     none
    in
        row[spy 15, wf]
        [
            column[spy 10, wf,  bw one]
            [
                row[bw 0, hf, bwb 1, wf, pdb 3]
                [
                    paragraph [bw one, fal, wf, bc 200 200 200, hpx 25, pd 5][textValue <| "Year"]
                ]
                ,column[spy 10, pdl 15, hf, scrollbarY, wf]
                (
                    Array.toList <| Array.indexedMap buildYearCheckboxes yearFilters -- column function needs List of item and not Array of items, so need conversion
                )
            ]
        ]


filterCheckBox : UIModel -> String -> Element Msg
filterCheckBox uiModel filterName =
        case filterName of
            "CDLNo" -> 
                checkbox [bw one, pdr 5 ] {
                    onChange = FilterCDLNoCheckBoxClicked
                    ,icon = buildChkBoxImage
                    , label = labelLeft [] none-- (el [] <| textValue chkboxLabel)
                    --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
                    , checked = uiModel.filterCDLNoSelected
                }

            "CDLYes" -> 
                checkbox [bw one, pdr 5 ] {
                    onChange = FilterCDLYesCheckBoxClicked
                    ,icon = buildChkBoxImage
                    , label = labelLeft [] none --(el [] <| textValue chkboxLabel)
                    --, checked = uiModel.filterSelectionsModel.filterCDLNoSelected
                    , checked = uiModel.filterCDLYesSelected
                }
            
            _ ->
                none

buildChkBoxImage userAction =
        if userAction == True then 
            image [hpx 24] {src = "checked.png", description ="Logo" }
        else 
            el [hpx 24, wpx 24, bw 2, br 5] <| none