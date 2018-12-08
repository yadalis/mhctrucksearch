module Helpers.ElmUI exposing (..)

import Element exposing (htmlAttribute, text, image, el, none)
import Helpers.ElmStyleShotcuts exposing (..)
import Html.Attributes exposing (id)

eId idValue
    = htmlAttribute (id idValue)

textValue stringValue
    = text <| stringValue

buildCollapseAllImage userAction =
    if userAction == True then 
        image [hpx 18, bw one] {src = "collapse.png", description ="Logo" }
    else 
        image [hpx 18, bw one] {src = "expand.png", description ="Logo" }

expandCollapseAll state =
    
        if state then
            hf
        else
            hpx 0

buildChkBoxImage userAction =
        if userAction == True then 
            image [hpx 16] {src = "checked.png", description ="Logo" }
        else 
            el [hpx 16, wpx 16, bw 1, br 3] <| none --this put empty square, unchecked checkbox style

one = 0

two = 0

new = 1