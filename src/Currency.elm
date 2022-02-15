module Currency exposing (Currency(..), allInputCurrency, toString)


type Currency
    = USD
    | EUR
    | GBP
    | CNY
    | JPY
    | WUC


toString : Currency -> String
toString cur =
    case cur of
        USD ->
            "USD"

        EUR ->
            "EUR"

        GBP ->
            "GBP"

        CNY ->
            "CNY"

        JPY ->
            "JPY"

        WUC ->
            "WUC"


allInputCurrency : List Currency
allInputCurrency =
    [ USD, EUR, GBP, CNY, JPY ]
