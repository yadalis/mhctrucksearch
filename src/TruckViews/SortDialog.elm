module TruckViews.SortDialog exposing (..)

import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Element exposing (..)
import Element.Font as Font exposing (..)
import Element.Input as Input exposing (..)
import Msg exposing (..)
import Model exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)

showSortOptionsDialog : Bool -> Element Msg
showSortOptionsDialog show =
    if show then 
        column[bc 245 245 245, pd 15, br 5, bw 2, spy 25]
        [
            row[Element.alignRight, bw 2, fac, bc 200 200 200  ][
                     Input.button ( [hf, bwb 0, fal, pdb 0])
                                        { 
                                            onPress = Just <| OperateSortDialog False
                                            ,label = textValue " X "
                                        }
            ],

            row[wf, pdt 0][
                column[spy 10, wf] --[
                    <|
                    
                        List.intersperse (el[bwb 1, wf] <| textValue "") (List.map (\(k, d, v) -> buildSortOption d v  ) sortByItemslist)
                    -- Input.button ( [hf, bwb 0, fal, pdb 0])
                    --                     { 
                    --                         onPress = Nothing
                    --                         ,label = textValue "Recently Added"
                    --                     }
                    -- , el[bwb 1, wf] <| textValue ""
                    -- ,Input.button ( [hf, bwb 0, fal, pdb 0])
                    --                     { 
                    --                         onPress = Just <| SortTrucks PriceLowToHigh
                    --                         ,label = textValue "Price - Lowest to Highest"
                    --                     }
                    -- , el[bwb 1, wf] <| textValue ""
                    -- ,Input.button ( [hf, bwb 0, fal, pdb 0])
                    --                     { 
                    --                         onPress = Just <| SortTrucks PriceHighToLow
                    --                         ,label = textValue "Price - Highest to Lowest"
                    --                     }
                    -- , el[bwb 1, wf] <| textValue ""
                    -- ,Input.button ( [hf, bwb 0, fal, pdb 0])
                    --                     { 
                    --                         onPress = Just <| SortTrucks MileageLowToHigh
                    --                         ,label = textValue "Mileage - Lowest to Highest"
                    --                     }
                    -- , el[bwb 1, wf] <| textValue ""
                    -- ,Input.button ( [hf, bwb 0, fal, pdb 0])
                    --                     { 
                    --                         onPress = Just <| SortTrucks MileageHighToLow
                    --                         ,label = textValue "Mileage - Highest to Lowest"
                    --                     }
                    -- , el[bwb 1, wf] <| textValue ""
                    -- ,Input.button ( [hf, bwb 0, fal, pdb 0])
                    --                     { 
                    --                         onPress = Nothing
                    --                         ,label = textValue "Nearest Location"
                    --                     }
                --]
            ]
        ]
        
    else
        none



buildSortOption label msg =
    Input.button ( [hf, bwb 0, fal, pdb 0])
                    { 
                        onPress = Just <| SortTrucks msg
                        ,label = textValue label
                    }
--, el[bwb 1, wf] <| textValue ""