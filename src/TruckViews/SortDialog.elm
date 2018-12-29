module TruckViews.SortDialog exposing (..)

import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..)
import Msg exposing (..)
import Model exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)

showSortOptionsDialog : Bool -> SortBy -> Element Msg
showSortOptionsDialog show currentSortByOption =
    if show then 
        column[bc 245 245 245, pd 15, br 5, bw 2, spy 0, wpx 300]
        [
            row[ear, bw 2, fac, bc 228 228 228  ][
                     Input.button ( [hf, bwb 0, fal, pdb 0])
                                        { 
                                            onPress = Just <| OperateSortDialog False
                                            ,label = textValue " x "
                                        }
            ],

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
                ([bc 175 175 175], [])
            else
                ([bc 245 245 245], [bc 225 225 225])
    in
        row ([wf, mouseOver mouseOverStyle ] ++ rowStyle)
        [
            Input.button ( [hf, bwb 0, fal, pdb 0, wf
                            ])
                            { 
                                onPress = Just <| SortTrucks msg
                                ,label = el[pd 3] <| textValue label
                            }
        ]