module View exposing (View, map, placeholder)

import Html as H exposing (Html)


type alias View msg =
    { title : String
    , body : List (Html msg)
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = List.map (H.map fn) doc.body
    }


placeholder : String -> View msg
placeholder moduleName =
    { title = moduleName
    , body = [ H.text moduleName ]
    }
