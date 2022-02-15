module Main exposing (main)

import Browser
import Currency exposing (Currency, allInputCurrency)
import Html exposing (Html, div, h1, input, label, option, select, span, text)
import Html.Attributes exposing (class, for, id, placeholder, type_)
import Html.Events exposing (onInput)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model, Cmd.none )

        UpdateCurrencyTime _ ->
            ( model, Cmd.none )

        ChangedCurrency maybeCur ->
            case maybeCur of
                Just cur ->
                    ( { model
                        | currentMoney =
                            { value = model.currentMoney.value
                            , denom = cur
                            }
                      }
                    , Cmd.none
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
                    , Cmd.none
                    )

                Nothing ->
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
                ++ String.fromFloat model.currentMoney.value
                ++ " "
                ++ Currency.toString model.currentMoney.denom
        ]


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
