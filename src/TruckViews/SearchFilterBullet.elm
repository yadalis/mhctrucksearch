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
                                row[ bw 0, pd 3, spx 0]
                                [
                                    column[][
                                        el [bw 1, Font.size 14, brc 97 97 97, Font.color <| rgb255 244 69 66,  bc 215 215 215,pd 3, Font.light] (textValue <| sf.searchFilterKey)
                                    ]
                                    ,column[][

                                                    Input.button ( [ hf, eId "X",bw 1, brc 97 97 97,  bc 97 97 97,pd 3])
                                                    { 
                                                        onPress = Just (RemoveSearchFilterItemFromPinnedSearchFilters sf) --if String.length uiModel.searchString > 0 then Just SearchPressed else Nothing
                                                        ,label = (el [Font.size 14,Font.color <| rgb255 250 250 250] <| textValue <| "x")
                                                    }


                                    ]
                                ]
                            else
                                none    
                    )
        |> Array.toList
        |> wrappedRow [   bw 0, wf, pdr 0]