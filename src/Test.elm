module Test exposing (main)

import Browser
import Browser.Events exposing (onClick, onKeyPress)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode exposing (string)
import Random
import String exposing (..)
import Task
import Time


type alias Model =
    { total : Float
    , input : Maybe String
    , lastValue : Maybe Float
    , selectedOperator : Maybe Operator
    , append : Bool
    , darkTheme : Bool
    , time : Time.Posix
    , zone : Time.Zone
    , key : String
    }


type Operator
    = Add
    | Minus
    | Times
    | Divide


init : () -> ( Model, Cmd Msg )
init _ =
    ( { total = 0
      , input = Nothing
      , lastValue = Nothing
      , selectedOperator = Nothing
      , append = True
      , darkTheme = False
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , key = ""
      }
    , Task.perform AdjustTimeZone Time.here
    )


maybeStringToMaybeFloat : Maybe String -> Maybe Float
maybeStringToMaybeFloat input =
    String.toFloat <| Maybe.withDefault "" input



-- onlyOK: Result err ok -> Maybe ok
-- onlyOk result =
--     case result of
--         Err _ ->
--             Nothing
--         Ok ok ->
--             Just ok
-- type Result x a
--     = Ok a
--     | Err x
-- Ok : a -> Result x a
-- Err : x -> Result x a


test : Result err ok -> Maybe ok
test =
    Result.map
        Just
        >> Result.withDefault Nothing



-- map: ( a -> b ) -> Result x a -> Result x b
--      ( a -> Maybe b ) -> Result x a -> Result x (Maybe b)
--      ( a -> b -> c ) -> Result x a -> Result x ( b -> c )
--      ( hej a -> fisk b ) -> Result (hej a) -> Result x ( fisk b )
-- a->b->c
-- Default a->(b->c)
-- (a->b)->c
-- make a -> Maybe a
-- make a =
--     Just a
-- make a -> Maybe a
-- make a =
--     Nothing
-- Result.map : (a -> b) -> Result x a -> Result x b
-- Byt ut symboler vilt försök inte vara smart
-- Tänk i algebra
-- Samma sak utan case men Result
--update


type Msg
    = None
    | DivideMsg
    | TimesMsg
    | MinusMsg
    | AddMsg
    | Equal
    | Decimal
    | Zero
    | Number Int
    | Clear
    | GenerateRandomNumber
    | RandomNumber Int
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | DarkMode
    | CharacterKey Char
    | ControlKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        appendDecimal : String -> String
        appendDecimal string =
            if String.contains "." string then
                string

            else
                string ++ "."
    in
    case msg of
        None ->
            ( model, Cmd.none )

        Clear ->
            ( { model | total = 0, lastValue = Nothing, input = Nothing }, Cmd.none )

        Number number ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ String.fromInt number, total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt number, append = True, total = 0 }, Cmd.none )

        Decimal ->
            if not (String.isEmpty (Maybe.withDefault "" model.input)) && model.append then
                ( { model | input = Just <| appendDecimal <| Maybe.withDefault "" model.input }, Cmd.none )

            else
                ( { model | input = Just "0.", append = True }, Cmd.none )

        Zero ->
            if String.isEmpty (Maybe.withDefault "" model.input) || not model.append then
                ( { model
                    | input = Just "0"
                    , append = False
                  }
                , Cmd.none
                )

            else
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "0" }, Cmd.none )

        DivideMsg ->
            ( { model
                | selectedOperator = Just Divide
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )

        TimesMsg ->
            ( { model
                | selectedOperator = Just Times
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )

        MinusMsg ->
            ( { model
                | selectedOperator = Just Minus
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )

        AddMsg ->
            ( { model
                | selectedOperator = Just Add
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )

        Equal ->
            let
                lastValue =
                    Maybe.withDefault 0 model.lastValue

                input =
                    Maybe.withDefault 0 <| maybeStringToMaybeFloat model.input

                sum =
                    case model.selectedOperator of
                        Nothing ->
                            0

                        Just Add ->
                            lastValue + input

                        Just Minus ->
                            lastValue - input

                        Just Times ->
                            lastValue * input

                        Just Divide ->
                            lastValue / input
            in
            if model.append then
                ( { model
                    | total = sum
                    , append = False
                    , selectedOperator = Nothing
                    , lastValue = Nothing
                    , input = Nothing
                  }
                , Cmd.none
                )

            else
                ( { model
                    | total = sum
                    , append = False
                    , selectedOperator = Nothing
                    , lastValue = Nothing
                    , input = Nothing
                  }
                , Cmd.none
                )

        -- Playing With Commands
        GenerateRandomNumber ->
            ( model, Random.generate RandomNumber (Random.int 0 1000) )

        RandomNumber number ->
            ( { model
                | input = Just <| String.fromInt number
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )

        -- Playing With Subscriptions
        Tick newTime ->
            ( { model
                | time = newTime
              }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model
                | zone = newZone
              }
            , Cmd.none
            )

        DarkMode ->
            let
                changeMode =
                    if model.darkTheme then
                        False

                    else
                        True
            in
            ( { model
                | darkTheme = changeMode
              }
            , Cmd.none
            )

        CharacterKey '1' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "1", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 1, append = True, total = 0 }, Cmd.none )

        CharacterKey '2' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "2", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 2, append = True, total = 0 }, Cmd.none )

        CharacterKey '3' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "3", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 3, append = True, total = 0 }, Cmd.none )

        CharacterKey '4' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "4", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 4, append = True, total = 0 }, Cmd.none )

        CharacterKey '5' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "5", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 5, append = True, total = 0 }, Cmd.none )

        CharacterKey '6' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "6", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 6, append = True, total = 0 }, Cmd.none )

        CharacterKey '7' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "7", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 7, append = True, total = 0 }, Cmd.none )

        CharacterKey '8' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "8", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 8, append = True, total = 0 }, Cmd.none )

        CharacterKey '9' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "9", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 9, append = True, total = 0 }, Cmd.none )

        CharacterKey '0' ->
            if model.append then
                ( { model | input = Just <| Maybe.withDefault "" model.input ++ "0", total = 0 }, Cmd.none )

            else
                ( { model | input = Just <| String.fromInt 0, append = True, total = 0 }, Cmd.none )

        CharacterKey '*' ->
            ( { model
                | selectedOperator = Just Times
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )

        CharacterKey '/' ->
            ( { model
                | selectedOperator = Just Divide
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )

        CharacterKey '+' ->
            ( { model
                | selectedOperator = Just Add
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )
        CharacterKey '-' ->
            ( { model
                | selectedOperator = Just Minus
                , lastValue = maybeStringToMaybeFloat model.input
                , append = False
              }
            , Cmd.none
            )

        ControlKey "Enter" ->
            let
                lastValue =
                    Maybe.withDefault 0 model.lastValue

                input =
                    Maybe.withDefault 0 <| maybeStringToMaybeFloat model.input

                sum =
                    case model.selectedOperator of
                        Nothing ->
                            0

                        Just Add ->
                            lastValue + input

                        Just Minus ->
                            lastValue - input

                        Just Times ->
                            lastValue * input

                        Just Divide ->
                            lastValue / input
            in
            if model.append then
                ( { model
                    | total = sum
                    , append = False
                    , selectedOperator = Nothing
                    , lastValue = Nothing
                    , input = Nothing
                  }
                , Cmd.none
                )

            else
                ( { model
                    | total = sum
                    , append = False
                    , selectedOperator = Nothing
                    , lastValue = Nothing
                    , input = Nothing
                  }
                , Cmd.none
                )
        

        _ ->
            ( model, Cmd.none )


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onKeyPress keyDecoder
        ]



-- VIEW


primaryButton : Msg -> String -> Html Msg
primaryButton onClick label =
    Html.button
        [ Html.Attributes.class "button number w-full bg-slate-100 dark:bg-slate-600 h-full dark:text-white"
        , Html.Events.onClick onClick
        ]
        [ Html.span
            []
            [ Html.text label ]
        ]


secondaryButton : Msg -> String -> Bool -> Html Msg
secondaryButton onClick label isTarget =
    Html.button
        [ Html.Attributes.classList
            [ ( "button operator w-full bg-slate-200 dark:bg-slate-800 h-full dark:text-white ", True )
            , ( "target", isTarget )
            ]
        , Html.Events.onClick onClick
        ]
        [ Html.span
            []
            [ Html.text label ]
        ]


clearButton : Msg -> String -> Html Msg
clearButton onClick label =
    Html.button
        [ Html.Attributes.class "button operator w-full bg-slate-200 dark:bg-slate-800 h-full dark:text-white"
        , Html.Events.onClick onClick
        ]
        [ Html.span
            []
            [ Html.text label ]
        ]


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)

        themeLabel =
            if model.darkTheme then
                "Light Mode"

            else
                "Dark Mode"
    in
    Html.table
        [ Html.Attributes.classList
            [ ( "table-fixed w-full h-screen border-collapse", True )
            , ( "dark bg-zinc-900", model.darkTheme )
            ]
        ]
        [ Html.thead
            []
            [ Html.tr
                []
                [ Html.th [ Html.Attributes.colspan 4 ]
                    [ Html.div
                        [ Html.Attributes.class "display" ]
                        [ Html.div
                            [ Html.Attributes.class "display-text h-20 bg-slate-200 dark:bg-slate-500 dark:text-white flex-display" ]
                            [ Html.div
                                []
                                [ Html.text "Result "
                                ]
                            , Html.div
                                []
                                [ Html.text <| String.fromFloat model.total
                                ]
                            ]
                        , Html.div
                            [ Html.Attributes.class "display-text h-20 bg-slate-300 dark:bg-slate-600 dark:text-white flex-display" ]
                            [ Html.div
                                []
                                [ Html.text "Last Value "
                                ]
                            , Html.div
                                []
                                [ Html.text <| String.fromFloat (Maybe.withDefault 0 model.lastValue)
                                ]
                            ]
                        , Html.div
                            [ Html.Attributes.class "display-text h-20 bg-slate-400 dark:bg-slate-700 dark:text-white flex-display" ]
                            [ Html.div
                                []
                                [ Html.text "Input "
                                ]
                            , Html.div
                                []
                                [ Html.text <| Maybe.withDefault "0" model.input
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Html.tbody
            []
            [ Html.tr
                []
                [ Html.td
                    [ Html.Attributes.class "bg-slate-200 dark:bg-slate-800 dark:text-white" ]
                    [ Html.h1 [] [ Html.text (hour ++ ":" ++ minute ++ ":" ++ second) ] ]
                , Html.td
                    []
                    [ secondaryButton GenerateRandomNumber "Random" False ]
                , Html.td
                    []
                    [ secondaryButton DarkMode themeLabel False ]
                , Html.td
                    []
                    [ clearButton Clear "Clear" ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ primaryButton (Number 7) "7" ]
                , Html.td
                    []
                    [ primaryButton (Number 8) "8" ]
                , Html.td
                    []
                    [ primaryButton (Number 9) "9" ]
                , Html.td
                    []
                    [ secondaryButton DivideMsg "÷" <| model.selectedOperator == Just Divide ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ primaryButton (Number 4) "4" ]
                , Html.td
                    []
                    [ primaryButton (Number 5) "5" ]
                , Html.td
                    []
                    [ primaryButton (Number 6) "6" ]
                , Html.td
                    []
                    [ secondaryButton TimesMsg "x" <| model.selectedOperator == Just Times ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ primaryButton (Number 1) "1" ]
                , Html.td
                    []
                    [ primaryButton (Number 2) "2" ]
                , Html.td
                    []
                    [ primaryButton (Number 3) "3" ]
                , Html.td
                    []
                    [ secondaryButton MinusMsg "-" <| model.selectedOperator == Just Minus ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ primaryButton Zero "0" ]
                , Html.td
                    []
                    [ secondaryButton Decimal "." False ]
                , Html.td
                    []
                    [ secondaryButton Equal "=" False ]
                , Html.td
                    []
                    [ secondaryButton AddMsg "+" <| model.selectedOperator == Just Add ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
