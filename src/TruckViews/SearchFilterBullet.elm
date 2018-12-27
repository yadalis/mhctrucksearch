module TruckViews.SearchFilterBullet exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Element exposing (..)
import Element.Input as Input
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Array  as Array exposing (..)

searchFilterBulletView : Array SearchFilterType -> Element Msg
searchFilterBulletView filterList =
    filterList
        |> Array.map (\sf ->
                            if sf.userAction then 
                                row[ bw 0, pd 3 ]
                                [
                                    column[][
                                        el [bw 0, fs 15, brc 97 97 97, fc  190 5 30,  bc 231 231 231 ,pd 6, fl, hf] (textValue <| 
                                                                                if sf.filterCategory == TruckType then
                                                                                    sf.searchFilterExtraData
                                                                                else
                                                                                    sf.searchFilterKey
                                                                            )
                                    ]
                                    ,column[][
                                        Input.button ( [ bw 0, brc 97 97 97,  bc 231 231 231 ,pd 6])
                                        { 
                                            onPress = Just (FilterCheckBoxClicked sf.index sf.filterCategory False )                                                       
                                            ,label = (el [wpx 15, hpx 15, fs 14,fac, fc 300 300 300, bc 150 150 150] <| textValue <| "x")
                                        }
                                    ]
                                ]
                            else
                                none    
                    )
        |> Array.toList
        |> wrappedRow [   bw 0, wf, pdr 0]

-- anySearchFilterBulletsApplied : Array SearchFilterType -> Bool
-- anySearchFilterBulletsApplied filterList =
--     Array.toList filterList
--         |> List.any (\sf -> sf.userAction == True)