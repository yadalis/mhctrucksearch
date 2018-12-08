module TruckViews.Truck exposing (..)

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

import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import Helpers.Utils exposing (..)

trucksView  : List Truck -> Element Msg
trucksView trucks =
    trucks
        |> List.indexedMap truckView
        |> column [hf, wf, spy 5]

truckView  : Int -> Truck -> Element Msg
truckView index truck =
    let
        logsToBrowswerDevTools = Debug.log "searchValues -> " ["truck func..."]    
    in
    
        row[bwb 0, wf, pdb 15, bc 245 245 245 , spx 3, hf ] --bc 47 48 49
        [
            column[wfmax 225, bw 0, hf, pdt 5]
            [
                image [bw one, pdl 0, hf] {src = "https://az832863.vo.msecnd.net/~/media/images/trucks/i0414681/i0414681_1.jpg?_=-1039260339&mw=2048&thn=0&w=1024", description ="Logo" }
            ]
            ,
            column[wf, hf, pd 5, spy 15]
            [
                row[]
                [
                    paragraph [Font.size 25, Font.bold, fc  244 66 95] [textValue <| truck.title]
                ]
                ,row[]
                [
                    paragraph [Font.size 20, Font.bold] [textValue <| " $" ++  String.fromInt truck.price ++ ".00"]
                ]
                ,row[spaceEvenly, hf, wf]
                [
                    column[bw 0, wf, hf, pd 5, spy 15]
                    [
                        paragraph[Font.light, fal][ textValue <| getTruckIdNumber truck]
                        ,paragraph[Font.light, fal][ textValue <| "Make: " ++  truck.make]
                        ,paragraph[Font.light, fal][ textValue <| "Model: " ++  truck.model]
                    ]
                    ,column[bw 0, wf, hf, pd 5, spy 15]
                    [
                        paragraph[Font.light, fal][ textValue <| "Sales Status: " ++  truck.salesStatus]
                        ,paragraph[Font.light, fal][ textValue <| "Sleeper Roof: " ++  truck.sleeperRoof]
                        ,paragraph[Font.light, fal][ textValue <| "Sleeper Bunk: " ++  truck.sleeperBunk]
                    ]
                ]
            ]
            -- ,
            -- column[wfp 4, bw 2, hf, pdl 15]
            -- []
        ]

getTruckIdNumber : Truck -> String
getTruckIdNumber truck =
    if truck.stockNumber > 0 then 
        "Stock Number: i0" ++  String.fromInt truck.stockNumber
    else if truck.appraisalNumber > 0 then 
        "Appraisal Number: A" ++  String.fromInt truck.appraisalNumber
    else
        "PO Number: " ++ truck.poNumber