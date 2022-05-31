module Test exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random
import String exposing (..)
import Task
import Time



-- MODEL


type alias Calculator =
    { add : Float -> Float -> Float
    , minus : Float -> Float -> Float
    , times : Float -> Float -> Float
    , divide : Float -> Float -> Float
    }


type alias Model =
    { displayValue : String
    , function : Float -> Float -> Float
    , lastValue : Maybe Float
    , append : Bool
    , darkTheme : Bool
    , time : Time.Posix
    , zone : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { displayValue = ""
      , function = \x y -> y
      , lastValue = Nothing
      , append = True
      , darkTheme = False
      , time = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )


parseFloat : String -> Float
parseFloat input =
    Maybe.withDefault 0 (String.toFloat input)



--update


type Msg
    = None
    | Divide
    | Times
    | Minus
    | Add
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        operation : (Float -> Float -> Float) -> Model
        operation function =
            { model
                | function = function
                , lastValue = Just <| parseFloat model.displayValue
                , append = False
            }

        calculator : Calculator
        calculator =
            { add = \x y -> x + y
            , minus = \x y -> x - y
            , times = \x y -> x * y
            , divide = \x y -> x / y
            }

        calculate : String
        calculate =
            model.function (Maybe.withDefault 0 model.lastValue) (parseFloat model.displayValue) |> String.fromFloat

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
            ( { model | displayValue = "", lastValue = Nothing }, Cmd.none )

        Number number ->
            if model.append then
                ( { model | displayValue = model.displayValue ++ String.fromInt number }, Cmd.none )

            else
                ( { model | displayValue = String.fromInt number, append = True }, Cmd.none )

        Decimal ->
            if not (String.isEmpty model.displayValue) && model.append then
                ( { model | displayValue = appendDecimal model.displayValue }, Cmd.none )

            else
                ( { model | displayValue = "0.", append = True }, Cmd.none )

        Zero ->
            if String.isEmpty model.displayValue || not model.append then
                ( { model
                    | displayValue = "0"
                    , append = False
                  }
                , Cmd.none
                )

            else
                ( { model | displayValue = model.displayValue ++ "0" }, Cmd.none )

        Divide ->
            ( operation calculator.divide, Cmd.none )

        Times ->
            ( operation calculator.times, Cmd.none )

        Minus ->
            ( operation calculator.minus, Cmd.none )

        Add ->
            ( operation calculator.add, Cmd.none )

        Equal ->
            if model.append then
                ( { model
                    | displayValue = calculate
                    , lastValue = Just <| parseFloat model.displayValue
                    , append = False
                  }
                , Cmd.none
                )

            else
                ( { model
                    | displayValue = calculate
                    , append = False
                  }
                , Cmd.none
                )

        -- Playing With Commands
        GenerateRandomNumber ->
            ( model, Random.generate RandomNumber (Random.int 0 1000) )

        RandomNumber number ->
            ( { model
                | displayValue = String.fromInt number
                , lastValue = Just <| parseFloat model.displayValue
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


primaryButton : Msg -> String -> Html Msg
primaryButton onClick label =
    Html.button
        [ Html.Attributes.class "button number w-full p-10 bg-slate-100 dark:bg-slate-600 h-full dark:text-white"
        , Html.Events.onClick onClick
        ]
        [ Html.span
            []
            [ Html.text label ]
        ]


secondaryButton : Msg -> String -> Html Msg
secondaryButton onClick label =
    Html.button
        [ Html.Attributes.class "button operator w-full p-10 bg-slate-200 dark:bg-slate-900 h-full dark:text-white"
        , Html.Events.onClick onClick
        ]
        [ Html.span
            []
            [ Html.text label ]
        ]


clearButton : Msg -> String -> Html Msg
clearButton onClick label =
    Html.button
        [ Html.Attributes.class "button operator w-full p-10 bg-slate-200 dark:bg-slate-900 h-full dark:text-white"
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
                [ ( "table-fixed w-full h-screen", True )
                , ( "dark", model.darkTheme )
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
                            [ Html.Attributes.class "display-text h-20 bg-slate-200 dark:bg-slate-600 dark:text-white" ]
                            [ case model.lastValue of
                                    Nothing ->
                                        Html.text ""
                                    Just lastValue->
                                        Html.text  <| String.fromFloat lastValue

                                 ]
                        , Html.div
                            [ Html.Attributes.class "display-text h-20 bg-slate-200 dark:bg-slate-600 dark:text-white" ]
                            [ Html.text model.displayValue ]
                        ]
                    ]
                ]
            ]
        , Html.tbody
            []
            [ Html.tr
                []
                [ Html.td
                    [ Html.Attributes.class "bg-slate-200 dark:bg-slate-900 dark:text-white" ]
                    [ Html.h1 [] [ Html.text (hour ++ ":" ++ minute ++ ":" ++ second) ] ]
                , Html.td
                    []
                    [ secondaryButton GenerateRandomNumber "Random" ]
                , Html.td
                    []
                    [ secondaryButton DarkMode themeLabel ]
                        
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
                    [ secondaryButton Divide "÷" ]
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
                    [ secondaryButton Times "x" ]
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
                    [ secondaryButton Minus "-" ]
                ]
            , Html.tr
                []
                [ Html.td
                    []
                    [ primaryButton Zero "0" ]
                , Html.td
                    []
                    [ secondaryButton Decimal "." ]
                , Html.td
                    []
                    [ secondaryButton Equal "=" ]
                , Html.td
                    []
                    [ secondaryButton Add "+" ]
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
