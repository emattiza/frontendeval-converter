module Main exposing (main)

import Browser
import Currency exposing (Currency, allInputCurrency)
import Html exposing (Html, div, input, label, option, select, span, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, field, float)
import Round
import Time


type alias Money =
    { value : Float
    , denom : Currency
    }


type Conversion
    = Conversion
        { sourceCurrency : Currency
        , targetCurrency : Currency
        , exchangeRate : Float
        }
    | Empty


type alias Model =
    { currentMoney : Money
    , currentExchangeRate : Conversion
    , previousExchangeRate : Conversion
    }


initialModel : Model
initialModel =
    { currentMoney =
        { value = 0
        , denom = Currency.USD
        }
    , currentExchangeRate = Empty
    , previousExchangeRate = Empty
    }


type Msg
    = Increment
    | UpdateCurrencyTime Time.Posix
    | ChangedCurrency (Maybe Currency)
    | ChangedAmount (Maybe Float)
    | GotNewRate (Result Http.Error Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model, Cmd.none )

        UpdateCurrencyTime _ ->
            ( model, getCurrencyRate model.currentMoney.denom )

        ChangedCurrency maybeCur ->
            case maybeCur of
                Just cur ->
                    ( { model
                        | currentMoney =
                            { value = model.currentMoney.value
                            , denom = cur
                            }
                      }
                    , getCurrencyRate cur
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangedAmount maybeAmt ->
            case maybeAmt of
                Just amt ->
                    ( { model
                        | currentMoney =
                            { value = amt
                            , denom = model.currentMoney.denom
                            }
                      }
                    , getCurrencyRate model.currentMoney.denom
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotNewRate rateResult ->
            case rateResult of
                Ok rate ->
                    ( { model
                        | previousExchangeRate = model.currentExchangeRate
                        , currentExchangeRate =
                            Conversion
                                { sourceCurrency = model.currentMoney.denom
                                , targetCurrency = Currency.WUC
                                , exchangeRate = rate
                                }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewCurrencyInputForm model
        , viewExchangeInfo model
        ]


viewExchangeInfo : Model -> Html Msg
viewExchangeInfo model =
    span
        [ class "exchange-output" ]
        [ text <|
            "Amount: "
                ++ (showConversion <| convertToWuc model.currentMoney model.currentExchangeRate)
                ++ " Change: "
                ++ showChange model.previousExchangeRate model.currentExchangeRate
        ]


showChange : Conversion -> Conversion -> String
showChange prev current =
    case prev of
        Empty ->
            ""

        Conversion sourceRecord ->
            case current of
                Empty ->
                    ""

                Conversion targetRecord ->
                    if sourceRecord.sourceCurrency == targetRecord.sourceCurrency then
                        ((targetRecord.exchangeRate
                            / sourceRecord.exchangeRate
                         )
                            - 1
                        )
                            * 100
                            |> Basics.round
                            |> (\x ->
                                    (if x > 0 then
                                        "↑"

                                     else if x < 0 then
                                        "↓"

                                     else
                                        ""
                                    )
                                        ++ String.fromInt x
                                        |> flip String.append "%"
                               )

                    else
                        ""


showConversion : Maybe Money -> String
showConversion maybeMoney =
    case Maybe.map showMoney maybeMoney of
        Just out ->
            out

        Nothing ->
            ""


showMoney : Money -> String
showMoney { value, denom } =
    Round.round 2 value ++ " " ++ Currency.toString denom


convertToWuc : Money -> Conversion -> Maybe Money
convertToWuc { value, denom } conversion =
    case conversion of
        Empty ->
            Nothing

        Conversion info ->
            if denom == info.sourceCurrency then
                Just { value = info.exchangeRate * value, denom = info.targetCurrency }

            else
                Nothing


viewCurrencyInputForm : Model -> Html Msg
viewCurrencyInputForm _ =
    div [ class "currency-input-form" ]
        [ label
            [ class "currency-input-label" ]
            [ span [] [ text "Amt" ]
            , input
                [ placeholder "0"
                , class "currency-input"
                , onInput (String.toFloat >> ChangedAmount)
                ]
                []
            , viewOptionsInputCurrency
            ]
        ]


viewOptionsInputCurrency : Html Msg
viewOptionsInputCurrency =
    select
        [ class "currency-select"
        , onInput (Currency.fromString >> ChangedCurrency)
        ]
        (List.map optionForCurrency allInputCurrency)


optionForCurrency : Currency -> Html Msg
optionForCurrency curr =
    option [] [ text <| Currency.toString curr ]


getCurrencyRate : Currency -> Cmd Msg
getCurrencyRate cur =
    Http.get
        { url = "https://api.frontendeval.com/fake/crypto/" ++ Currency.toString cur
        , expect = Http.expectJson GotNewRate rateDecoder
        }


rateDecoder : Decoder Float
rateDecoder =
    field "value" float


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        -- refresh every 10s (10,000 ms)
        duration =
            10 * 1000
    in
    Time.every duration UpdateCurrencyTime


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b
