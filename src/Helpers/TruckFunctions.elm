module Helpers.TruckFunctions exposing (..)

import Element exposing (..)
import Element.Input exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import Model exposing (..)
import Msg exposing (..)
--import List.Unique exposing (..)

buildCDLValueList : List Truck -> List String
buildCDLValueList trucks =
    List.map (\t -> t.cdl) trucks
        --|> filterDuplicates

-- buildCDLCheckboxList : Int -> List Truck -> Element Msg
-- buildCDLCheckboxList trucks =
--     List.map buildCDLCheckboxList trucks


buildCDLValueGroups : UIModel -> List Truck -> Element Msg
buildCDLValueGroups uiModel trucks =
    let
        cdlList = buildCDLValueList trucks
        
        cdlNoList =     List.filter (\cdl -> String.trim cdl == "No") cdlList

        cdlYesList =    List.filter (\cdl -> String.trim cdl == "Yes") cdlList


        -- cdlNoCheckboxLabel = "No (" ++  (String.fromInt <| (List.length cdlNoList))  ++ ")"
        -- cdlYesCheckboxLabel = "Yes (" ++  (String.fromInt <| (List.length cdlYesList))  ++ ")"

        cdlNoCheckboxLabel = "Nodd"
        cdlYesCheckboxLabel = "Yesdd"
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
                        filterCheckBox  uiModel "CDLNo" cdlNoCheckboxLabel
                        , textValue <| "No (" ++  (String.fromInt <| (List.length cdlNoList))  ++ ")"
                    ]
                    ,row[bw two]
                    [
                        filterCheckBox  uiModel "CDLYes" cdlYesCheckboxLabel
                        , textValue <| "Yes (" ++  (String.fromInt <| (List.length cdlYesList))  ++ ")"
                    ]
                ]
            ]
        ]

filterCheckBox : UIModel -> String -> String -> Element Msg
filterCheckBox uiModel filterName chkboxLabel =
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