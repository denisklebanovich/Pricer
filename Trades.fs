module Trades
open Payment
open EuropeanOption

type Trade = Payment of PaymentRecord | EuropeanOption of EuropeanOptionRecord

type TradeID = System.Guid

let newTradeID () : TradeID= System.Guid.NewGuid()
