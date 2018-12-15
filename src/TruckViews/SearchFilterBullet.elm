module TruckViews.SearchFilterBullet exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Commands exposing (..)
--import RemoteData  exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)
import Array  as Array exposing (..)
import Helpers.ElmStyleShotcuts as ElmUIShortCuts exposing (..)

searchFilterBulletView : Array SearchFilterType -> Element Msg
searchFilterBulletView filterList =
    filterList
        |> Array.map (\sf -> --el [bw 1, brc 185 185 185,  bc 195 195 195,pd 3] (
                            if sf.userAction then 
                                row[ bw 0, pd 3 ]
                                [
                                    column[][
                                        --el [bw 1, Font.size 14, brc 97 97 97, fc  250 250 250,  bc 105 105 125,pd 5, Font.light] (textValue <| sf.searchFilterKey)
                                        el [bw 0, Font.size 15, brc 97 97 97, fc  190 5 30,  bc 231 231 231 ,pd 6, Font.light, hf] (textValue <| sf.searchFilterKey)
                                    ]
                                    ,column[][
                                        Input.button ( [ bw 0, brc 97 97 97,  bc 231 231 231 ,pd 6])
                                        { 
                                            onPress = Just (FilterCheckBoxClicked sf.index sf.filterCategory False )                                                       
                                            ,label = (el [wpx 15, hpx 15, Font.size 14,centerX, centerY, fc 300 300 300, bc 150 150 150] <| textValue <| "x")
                                        }
                                    ]
                                ]
                            else
                                none    
                    )
        |> Array.toList
        |> wrappedRow [   bw 0, wf, pdr 0]

anySearchFilterBulletsApplied : Array SearchFilterType -> Bool
anySearchFilterBulletsApplied filterList =
    Array.toList filterList
        |> List.any (\sf -> sf.userAction == True)
        