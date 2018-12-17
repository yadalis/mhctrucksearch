module TruckViews.Truck exposing (..)

import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Element exposing (..)
import Helpers.ElmStyleShotcuts exposing (..)
import Helpers.ElmUI exposing (..)
import BusinessFunctions.TruckFunctions exposing (..)
import Numeral exposing(format, formatWithLanguage)
import Url.Builder exposing (..)

trucksView  : List Truck -> Element Msg
trucksView trucks =
    trucks
        |> List.indexedMap truckView
        |> column [hf, wf, spy 5]

truckView  : Int -> Truck -> Element Msg
truckView index truck =
    let
        isTruckSold  =
            truck.salesStatusFlag == "I" || truck.salesStatusFlag == "S"

        formatImageLink = truck.primaryImageLink
                            |> String.isEmpty
                            |> (\isLinkEmpty -> if isLinkEmpty then "photoscomingsoon.png"  else truck.primaryImageLink)
                            |> String.replace "&h=16" "&h=200"
                            |> String.replace "&w=16" "&w=200" 
                            |> String.replace "&thn=1" "&thn=2"
    in
    
        row[bwb 0, wf, pd 5, bc 240 240 240, hf ]
        [
            column[  bw 0, hf, pdt 0]
            [
                image [ bwl 0, pdl 0, bw 0, bc 250 250 250
                    ,

                    inFront (
                        if isTruckSold then
                            ( el[alignBottom, fc 250 250 250, bc 234 67 82, wf, alpha 0.55, feb] <| textValue "SOLD" )
                        else
                            none
                    )
                        ] {src = formatImageLink, description ="Logo" }
            ]
            ,
            column[wf, hf, pde 5 5 5 15, spy 15]
            [
                column[]
                [
                    link [wf,  Element.htmlAttribute (target "_blank"), fal ]
                        { url = 
                            crossOrigin "https://www.mhc.com/trucks/used" [truck.year, truck.make, truck.model, ((\tup -> Tuple.second tup ) <| buildTruckIdNumber truck)] []
                            , label = paragraph [fs 28, fb, fc  190 5 30] [textValue <| truck.title]
                        }
                    ,
                    -- link [wf,  Element.htmlAttribute (target "_blank"), fal ]
                    --     { url = 
                    --         formatImageLink
                    --         , label = paragraph [fs 28, fb, fc  190 5 30] [textValue <| formatImageLink]
                    --     }
                    -- ,
                        if truck.stockNumber == 0 then
                            paragraph [fs 22, fc  167 167 167, pdt 5, bw 0, fal] 
                                [textValue <| "Appraisal# - " ++ String.fromInt truck.appraisalNumber]
                        else
                            none
                ]
                ,row[]
                [
                    paragraph [fs 26, fb, fc 68 68 68] [textValue <| 
                        buildPriceValue truck.price                        
                    ]
                ]
                ,row[hf, wf, fs 16]
                [
                    column[bw 0, wfmax 350, hf, pd 0, spy 5]
                    [
                        dataFieldView "Location:" <| 
                                                    if String.isEmpty truck.locationName then
                                                        "N/A" 
                                                    else
                                                        truck.locationName

                        ,dataFieldView "Stock#:" <| if truck.stockNumber == 0 then "N/A" else String.fromInt truck.stockNumber
                        ,dataFieldView "Chassis#:" truck.chassisNumber
                        ,dataFieldView "Mileage:" <|  format "0,0" <|  truck.mileage
                        ,dataFieldView "Sleeper Size:" 
                                                        <| case String.toInt truck.sleeperInches of
                                                                    Just num -> String.fromInt num ++ " Inch"
                                                                    Nothing -> truck.sleeperInches
                    ]
                    ,column[bw 0,  wf, hf, pd 0, spy 5]
                    [
                        dataFieldView  "Engine Make:"   truck.engineMake
                        ,dataFieldView  "Engine Model:"   truck.engineModel
                        ,dataFieldView  "Horsepower:" <| String.fromFloat truck.engineHP
                        ,dataFieldView  "Transmission:"    truck.transType
                    ]
                ]
            ]
            -- show comments may be ? or use this for extra features like pop open for full details
            -- column[wfp 4, bw 2, hf, pdl 15]
            -- []
        ]

dataFieldView fieldName fieldValue =
    row[bw 0, wf]
    [
        paragraph[fal,spy 1, fs 18,  fc 105 105 105]
        [
            el[fb ] <| textValue <| fieldName
            , el[pdl 5, fs 16, fc 97 97 97] <| textValue <| fieldValue
        ]   
    ]

buildPriceValue : Float -> String
buildPriceValue price =
    if price == 0 then
        "Call for pricing"
    else
       format "$0,0" <| price