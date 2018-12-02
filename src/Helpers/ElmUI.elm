module Helpers.ElmUI exposing (..)

import Element exposing (htmlAttribute, text)
import Html.Attributes exposing (id)

eId idValue
    = htmlAttribute (id idValue)

textValue stringValue
    = text <| stringValue

one = 0

two = 0

new = 1