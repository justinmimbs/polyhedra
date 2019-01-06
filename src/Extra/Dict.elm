module Extra.Dict exposing (lookup)

import Dict exposing (Dict)


lookup : v -> Dict comparable v -> comparable -> v
lookup default dict key =
    case Dict.get key dict of
        Just value ->
            value

        Nothing ->
            default
