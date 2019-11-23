module Json.Decode.Extra exposing
    ( date
    , iso8601
    , nothing
    , posix
    , timezone
    )

import Dict
import Iso8601
import Json.Decode exposing (..)
import Time exposing (Month, Posix, Zone, millisToPosix)
import Time.Extra as Time
import TimeZone


nothing : Decoder ()
nothing =
    value |> map (\_ -> ())


{-| Decode a JSON float value representing the number of seconds since epoch
into a `Posix`.

    import Json.Decode exposing (..)
    import Time

    decodeString (field "created_at" posix)
        "{ \"created_at\": 1574447205.394}"
        --> Ok (Time.millisToPosix 1574447205000)

-}
posix : Decoder Posix
posix =
    map (truncate >> (*) 1000 >> millisToPosix) float


{-| Decode an ISO8601 JSON string into a `Posix`.

    import Json.Decode exposing (..)
    import Time


    decodeString (field "created_at" iso8601)
        "{ \"created_at\": \"2019-11-22T18:26:45Z\"}"
        --> Ok (Time.millisToPosix 1574447205000)

-}
iso8601 : Decoder Posix
iso8601 =
    Iso8601.decoder


{-| Decode a timezone JSON string value into a `Zone`.

    import Json.Decode exposing (..)
    import TimeZone

    decodeString (field "timezone" timezone)
        "{ \"timezone\": \"America/Los_Angeles\"}"
        --> Ok ("America/Los_Angeles", TimeZone.america__los_angeles ())

-}
timezone : Decoder ( String, Zone )
timezone =
    string
        |> andThen
            (\x ->
                case Dict.get x TimeZone.zones of
                    Just tz ->
                        succeed ( x, tz () )

                    Nothing ->
                        fail ("Unknown timezone \"" ++ x ++ "\"")
            )


{-| Decode an ISO8601 JSON string into a `Posix`.

    import Json.Decode exposing (..)
    import Time

    decodeString (field "created_at" date) "{ \"created_at\": \"2019-11-22\"}"
        --> Ok (2019, Time.Nov, 22)

-}
date : Decoder ( Int, Month, Int )
date =
    string
        |> andThen
            (\value ->
                case String.split "-" value of
                    [ y_, m_, d_ ] ->
                        Maybe.map3
                            (\a b c -> succeed ( a, b, c ))
                            (String.right 4 y_ |> String.toInt)
                            (String.right 2 m_
                                |> String.toInt
                                |> Maybe.andThen Time.intToMonth
                            )
                            (String.right 2 d_ |> String.toInt)
                            |> Maybe.withDefault (fail ("Unknown dat value: " ++ value))

                    _ ->
                        fail ("Unknown date value: " ++ value)
            )
