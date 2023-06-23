module Valuation

open EuropeanOption
open Trades
open Payment
open Option

let valuateTrade config marketData (trade: Trade) : Trade =
    match trade with
    | Payment p ->
        let inputs: PaymentValuationInputs =
            { Trade = p
              Data = config
              MarketData = marketData }

        let vm = PaymentValuationModel(inputs)

        Payment
            { p with
                Value = Some <| vm.Calculate() }
    | EuropeanOption eo ->
        let inputs: EuropeanOptionValuationInputs =
            { Trade = eo
              Data = config
              MarketData = marketData }

        let vm = EuropeanOptionValuationModel(inputs)
        let result = vm.Calculate()
        let value = fst result
        let delta = snd result

        EuropeanOption
            { eo with
                Value = Some value
                Delta = Some delta }
