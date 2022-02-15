module Currency exposing (Currency(..), allInputCurrency, toString, fromString)


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

fromString : String -> Maybe Currency
fromString cur =
    case cur of
        "USD" -> Just USD
        "EUR" -> Just EUR
        "GBP" -> Just GBP
        "CNY" -> Just CNY
        "JPY" -> Just JPY
        "WUC" -> Just WUC
        _ -> Nothing


allInputCurrency : List Currency
allInputCurrency =
    [ USD, EUR, GBP, CNY, JPY ]
