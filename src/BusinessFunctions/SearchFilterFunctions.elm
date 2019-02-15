module BusinessFunctions.SearchFilterFunctions exposing (..)

import Model exposing (..)
import Array exposing (..)
import List.Extra exposing (..)
import List.Unique exposing (..)
import Maybe.Extra exposing (..)
 
resetFilters filters = 
        Array.map(\sf -> {sf | userAction = False } ) filters

anyFilterApplied uiModel =
        List.length (uiModel.selectedFilterBullets) > 0

partialSearchFiltersMetadata = 
    [
         {filterName = Condition        , filterCode = "cn"  ,  displayText = "New/Used"         , filterNameString = "Condition"               }
        ,{filterName = FleetCode        , filterCode = "fc"  ,  displayText = "Fleet Code"        , filterNameString = "FleetCode"               }
        ,{filterName = SalesStatus      , filterCode = "ss"  ,  displayText = "Sales Status"      , filterNameString = "SalesStatus"             }
        ,{filterName = TruckType        , filterCode = "tt"  ,  displayText = "Truck Status"      , filterNameString = "TruckType"               }
        ,{filterName = SpecialFinancing , filterCode = "sf"  ,  displayText = "Special Financing" , filterNameString = "SpecialFinancing"        }
        ,{filterName = Price            , filterCode = "pr"  ,  displayText = "Price"             , filterNameString = "Price"                   }
        ,{filterName = Mileage          , filterCode = "ml"  ,  displayText = "Mileage"           , filterNameString = "Mileage"                 }
        ,{filterName = Year             , filterCode = "yr"  ,  displayText = "Year"              , filterNameString = "Year"                    }
        ,{filterName = Make             , filterCode = "mk"  ,  displayText = "Make"              , filterNameString = "Make"                    }
        ,{filterName = MakeModel        , filterCode = "mm"  ,  displayText = "Model"             , filterNameString = "MakeModel"               }
        ,{filterName = EngineMake       , filterCode = "em"  ,  displayText = "Engine"            , filterNameString = "EngineMake"              }
        ,{filterName = EngineHP         , filterCode = "eh"  ,  displayText = "HP"                , filterNameString = "EngineHP"                }
        ,{filterName = TransType        , filterCode = "ts"  ,  displayText = "Transmission"      , filterNameString = "TransType"               }
        ,{filterName = SleeperRoof      , filterCode = "sr"  ,  displayText = "Sleeper Roof"      , filterNameString = "SleeperRoof"             }
        ,{filterName = SleeperBunk      , filterCode = "sb"  ,  displayText = "Sleeper Bunk"      , filterNameString = "SleeperBunk"             }
        ,{filterName = SleeperInches    , filterCode = "si"  ,  displayText = "Sleeper Size"      , filterNameString = "SleeperInches"           }
        ,{filterName = Suspension       , filterCode = "su"  ,  displayText = "Suspension"        , filterNameString = "Suspension"              }
        ,{filterName = WheelBase        , filterCode = "wb"  ,  displayText = "Wheel Base"        , filterNameString = "WheelBase"               }        
        ,{filterName = RearAxleType     , filterCode = "ra"  ,  displayText = "Rear Axle Type"    , filterNameString = "RearAxleType"            }
        ,{filterName = RearAxleRatio    , filterCode = "rr"  ,  displayText = "Rear Axle Ratio"   , filterNameString = "RearAxleRatio"           }      
        ,{filterName = FrontAxleWeight  , filterCode = "fw"  ,  displayText = "Front Axle Weight" , filterNameString = "FrontAxleWeight"         }
        ,{filterName = RearAxleWeight   , filterCode = "rw"  ,  displayText = "Rear Axle Weight"  , filterNameString = "RearAxleWeight"          }
        ,{filterName = RearWheelSize    , filterCode = "rs"  ,  displayText = "Rear Wheel Size"   , filterNameString = "RearWheelSize"           }      
        ,{filterName = FrontWheelSize   , filterCode = "fs"  ,  displayText = "Front Wheel Size"  , filterNameString = "FrontWheelSize"          }       
        ,{filterName = BrakeType        , filterCode = "bk"  ,  displayText = "Brake Type"        , filterNameString = "BrakeType"               }
        ,{filterName = ExhaustType      , filterCode = "et"  ,  displayText = "Exhaust Type"      , filterNameString = "ExhaustType"             }
        ,{filterName = BodyType         , filterCode = "bt"  ,  displayText = "Body Type"         , filterNameString = "BodyType"                }
        ,{filterName = APU              , filterCode = "ap"  ,  displayText = "APU"               , filterNameString = "APU"                     }
        ,{filterName = CDL              , filterCode = "cd"  ,  displayText = "CDL"               , filterNameString = "CDL"                     }
        ,{filterName = Photo            , filterCode = "hp"  ,  displayText = "Photo"             , filterNameString = "HasPhoto"                   }      
        ,{filterName = InventoryAge     , filterCode = "ia"  ,  displayText = "Inventory Age"     , filterNameString = "InventoryAge"            }
        ,{filterName = LocationName     , filterCode = "lc"  ,  displayText = "Location Name"     , filterNameString = "LocationName"            }
        ,{filterName = OwningBranch     , filterCode = "ob"  ,  displayText = "Owning Branch"     , filterNameString = "OwningBranch"            }     
    ]

formattedSelectedFilterBullets uiModel = 
        if List.length uiModel.selectedFilterBullets > 0 then
        partialSearchFiltersMetadata
                |> List.map 
                                (
                                \eachFilterType -> 
                                        uiModel.selectedFilterBullets
                                                |> List.filter (\selectedFilterBullet -> selectedFilterBullet.filterCategory == eachFilterType.filterName) 
                                                |> List.map (\eachBullet -> eachBullet.searchFilterExtraData )
                                                |> String.join ","
                                                |> \finalStr -> 
                                                                if String.length finalStr > 0 then 
                                                                        eachFilterType.filterCode ++ "=" ++ finalStr
                                                                else
                                                                        ""
                                )
        else
        []