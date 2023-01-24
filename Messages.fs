module Messages

open Configuration
open Model
open System

//this is currently payment specific, once we add other trades we will need to re-model this
//other trades will have fields that payment doesn't have
type TradeChangeMsg =
    | NewName of Guid * string
    | NewPrincipal of Guid * string
    | NewCurrency of Guid * string
    | NewExpiry of Guid * string

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | TradeChange of TradeChangeMsg
    | ConfigChange of string * string
    | MarketDataChange of string * string
    | AddPayment
    | RemoveTrade of Guid
    | RecalculateAll
    | GotMarketData of JsonConfig
    | GetConfig
    | GotConfig of JsonConfig
    | Warning of string
    | Error of exn
    | ClearError
