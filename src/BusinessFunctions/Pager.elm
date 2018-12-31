module BusinessFunctions.Pager exposing (..)

import List.Extra exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..) 
import Msg exposing (..)
import Helpers.ElmUI exposing (..)

buildPageNumbersView  filteredTruckList currentPageNumber = 
    let
        grps = greedyGroupsOf 100 filteredTruckList
        pageNumbers = (List.range 1  <| List.length grps)

        searchStringBtnStyle num = 
                    if currentPageNumber == num then 
                        [  bwb 0, bc 185 185 185, fc 57 57 57 , fs 16]
                    else
                        [   bwb 0, fc 244 66 95  , fs 12]
    in
    
        if List.length pageNumbers > 1 then
            List.map (\num -> 
                           row[wpx 25, hpx 20]
                                    [
                                        Input.button ([
                                                        if currentPageNumber /= num then
                                                            mouseOver [ bc  0 0 0, fc 250 250 250  ]
                                                        else
                                                            mouseOver [ bc  175 175 175 ]
                                                        ,
                                                        pd 0, wf, hf,    fb ] ++ (searchStringBtnStyle num))
                                            { 
                                                onPress = Just (PageNumberClicked num )
                                                ,label =  el[eacx,eacy] <| textValue <| String.fromInt num
                                            }
                                    ]

                    ) pageNumbers
        else
            [none]