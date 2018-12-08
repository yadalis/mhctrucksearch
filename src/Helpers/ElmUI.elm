module Helpers.ElmUI exposing (..)

import Element exposing (htmlAttribute, text, image)
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

one = 0

two = 0

new = 1