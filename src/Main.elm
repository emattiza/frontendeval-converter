module Main exposing (main)

import Browser
import Currency exposing (Currency)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Time


type Money
    = Money
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
        Money
            { value = 0
            , denom = Currency.WUC
            }
    , currentExchangeRate = Empty
    , previousExchangeRate = Empty
    }


type Msg
    = Increment
    | UpdateCurrencyTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model, Cmd.none )

        UpdateCurrencyTime _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    div [ class "container" ]
        []


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
