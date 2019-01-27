module TruckViews.SortDialog exposing (..)

import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Colors exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..)
import Msg exposing (..)
import Model exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)

showSortOptionsDialog : Bool -> SortMetaData -> Element Msg
showSortOptionsDialog show currentSortByChoice =
    if show then 
        column[greyBg 245, pd 15, br 5, bw 2, wpx 300]
        [
            row[ear, bw 1, greyBg 228  ][
                     Input.button []
                                        { 
                                            onPress = Just <| OperateSortDialog False
                                            ,label = textValue " x "
                                        }
            ]
            ,
            row[wf, pdt 10]
            [
                column[spy 3, wf]
                    <|
                        List.intersperse (el[bwb 1,bdot, wf] <| textValue "") (List.map (\sortMetaDataItem -> buildSortOption sortMetaDataItem currentSortByChoice.sortByField) sortByItemslist)
            ]
        ]
        
    else
        none

buildSortOption sortMetaDataItem currentSortByField =
    let
        (rowStyle, mouseOverStyle) =
            if sortMetaDataItem.sortByField == currentSortByField then
                ([greyBg 175], [])
            else
                ([greyBg 245], [greyBg 225])
    in
        row ([wf, mouseOver mouseOverStyle ] ++ rowStyle)
        [
            Input.button ( [hf, fal, wf
                            ])
                            { 
                                onPress = Just <| SortTrucks sortMetaDataItem
                                ,label = el[pd 3] <| textValue sortMetaDataItem.sortItemDisplayText
                            }
        ]