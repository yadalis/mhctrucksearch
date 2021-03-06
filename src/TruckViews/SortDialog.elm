module TruckViews.SortDialog exposing (..)

import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Colors exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..)
import Msg exposing (..)
import Model exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)

showSortOptionsDialog : Bool -> SortBy -> Element Msg
showSortOptionsDialog show currentSortByOption =
    if show then 
        column[greyBg 245, pd 15, br 5, bw 2, wpx 300]
        [
            row[ear, bw 1, greyBg 228  ][
                     Input.button ( [])
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
                        List.intersperse (el[bwb 1,bdot, wf] <| textValue "") (List.map (\(key, label, msg) -> buildSortOption key label msg currentSortByOption) sortByItemslist)
            ]
        ]
        
    else
        none

buildSortOption key label msg currentSortByOption =
    let
        (rowStyle, mouseOverStyle) =
            if key == convertSortByToKey currentSortByOption then
                ([greyBg 175], [])
            else
                ([greyBg 245], [greyBg 225])
    in
        row ([wf, mouseOver mouseOverStyle ] ++ rowStyle)
        [
            Input.button ( [hf, fal, wf
                            ])
                            { 
                                onPress = Just <| SortTrucks msg
                                ,label = el[pd 3] <| textValue label
                            }
        ]