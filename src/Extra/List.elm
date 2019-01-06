module Extra.List exposing (find, foldCycle)


find : (a -> Bool) -> List a -> Maybe a
find pred list =
    case list of
        x :: rest ->
            if pred x then
                Just x

            else
                find pred rest

        [] ->
            Nothing


{-| Left fold over each item with the next, and the last item with the first.
-}
foldCycle : (a -> a -> b -> b) -> b -> List a -> b
foldCycle f2 result list =
    case list of
        head :: _ ->
            foldCycleHelp head f2 result list

        [] ->
            result


foldCycleHelp : a -> (a -> a -> b -> b) -> b -> List a -> b
foldCycleHelp head f2 result list =
    case list of
        x :: ((y :: _) as rest) ->
            foldCycleHelp head f2 (f2 x y result) rest

        last :: [] ->
            f2 last head result

        [] ->
            result
