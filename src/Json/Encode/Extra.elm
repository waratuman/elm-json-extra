module Json.Encode.Extra exposing (iso8601, posix, timezone, maybe)

{-| Helper functions for the `Json.Decode` module of `elm/json`.

@docs iso8601, posix, timezone, maybe

-}

import Iso8601
import Json.Encode exposing (..)
import Time exposing (Posix, Zone, posixToMillis)


{-| Encode a `Posix` value into a JSON float representing the number of seconds
since epoch.

    import Json.Encode exposing (..)
    import Time

    encode 0
        ( object
            [ ( "created_at", posix (Time.millisToPosix 1574447205 ) ) ]
        )
        --> "{\"created_at\":1574447.205}"

-}
posix : Posix -> Value
posix =
    posixToMillis >> toFloat >> (\x -> x / 1000) >> float


{-| Encode a `Posix` value into a ISO8601 JSON string.

    import Json.Encode exposing (..)
    import Time

    encode 0
        ( object
            [ ( "created_at", iso8601 (Time.millisToPosix 1574447205394 ) ) ]
        )
        --> "{\"created_at\":\"2019-11-22T18:26:45.394Z\"}"

-}
iso8601 : Posix -> Value
iso8601 =
    Iso8601.encode


{-| Encode a `( String, Zone )` value into a JSON string with the
value being the name of the timezone.

    import Json.Encode exposing (..)
    import TimeZone

    let
        losAngeles =
            ("America/Los_Angeles", TimeZone.america__los_angeles ())
    in
    encode 0
        ( object
            [ ( "timezone", timezone losAngeles ) ]
        )
        --> "{\"timezone\":\"America/Los_Angeles\"}"

-}
timezone : ( String, Zone ) -> Value
timezone ( n, _ ) =
    string n


{-| Encode a `Maybe` value with the given encoder.

    import Json.Encode exposing (..)

    encode 0 (maybe int Nothing)
        --> "null"

    encode 0 (maybe int (Just 1))
        --> "1"

-}
maybe : (a -> Value) -> Maybe a -> Value
maybe fn value =
    case value of
        Just a ->
            fn a

        Nothing ->
            null
