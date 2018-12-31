module TruckViews.SearchFilterBullet exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Element exposing (..)
import Element.Input as Input
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Array  as Array exposing (..)
import Helpers.Colors exposing (..)

searchFilterBulletView : Array SearchFilterType -> Element Msg
searchFilterBulletView filterList =
    filterList
        |> Array.map (\sf ->
                            if sf.userAction then 
                                row[pd 3 ]
                                [
                                    column[][
                                        el [fs 15, greyBorder 97, mhcRed,  greyBg 231 ,pd 6, fl, hf] (textValue <| 
                                                                                if sf.filterCategory == TruckType then
                                                                                    sf.searchFilterExtraData
                                                                                else
                                                                                    sf.searchFilterKey
                                                                            )
                                    ]
                                    ,column[][
                                        Input.button ( [greyBorder 97,  greyBg 231  ,pd 6])
                                        { 
                                            onPress = Just (FilterCheckBoxClicked sf.index sf.filterCategory False )                                                       
                                            ,label = (el [wpx 15, hpx 15, fs 14,fac, greyFont 300, greyBg 150] <| textValue <| "x")
                                        }
                                    ]
                                ]
                            else
                                none    
                    )
        |> Array.toList
        |> wrappedRow [wf]

-- anySearchFilterBulletsApplied : Array SearchFilterType -> Bool
-- anySearchFilterBulletsApplied filterList =
--     Array.toList filterList
--         |> List.any (\sf -> sf.userAction == True)