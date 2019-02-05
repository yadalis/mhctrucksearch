module BusinessFunctions.Pager exposing (..)

import List.Extra exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..) 
import Msg exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Colors exposing (..)

buildPageNumbersView  filteredTruckList currentPageNumber = 
    let
        grps = greedyGroupsOf 100 filteredTruckList
        pageNumbers = (List.range 1  <| List.length grps)

        searchStringBtnStyle num = 
                    if currentPageNumber == num then 
                        [  greyBg 185, greyFont 57 , fs 16]
                    else
                        [  mhcMediumRed  , fs 12]
    in
        if List.length pageNumbers > 1 then
            List.map (\num -> 
                           row[wpx 25, hpx 20]
                                    [
                                        Input.button ([
                                                        if currentPageNumber /= num then
                                                            mouseOver [ greyBg 0, greyFont 250]
                                                        else
                                                            mouseOver [ greyBg 175 ]
                                                        ,
                                                        wf, hf, fb ] ++ (searchStringBtnStyle num))
                                            { 
                                                onPress = Just (PageNumberClicked num )
                                                ,label =  el[eacx,eacy] <| textValue <| String.fromInt num
                                            }
                                    ]

                    ) pageNumbers
        else
            [none]