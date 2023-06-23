module EuropeanOption

open System
open Configuration
open Microsoft.FSharp.Core
open Money
open MathNet.Numerics.Distributions

type OptionType =
    | Call
    | Put

let parseOptionType (input: string) =
    match input with
    | "Call" -> Some Call
    | "Put" -> Some Put
    | _ -> None

type ValuationMethod =
    | Analytical
    | MonteCarlo

let parseValuationMethod (input: string) =
    match input with
    | "Analytical" -> Some Analytical
    | "MonteCarlo" -> Some MonteCarlo
    | _ -> None

(* Model for European Option trade. *)
type EuropeanOptionRecord =
    { TradeName: string
      SpotPrice: float
      Strike: float
      Drift: float
      Volatility: float
      Expiry: DateTime
      Currency: string
      ValuationMethod: ValuationMethod
      OptionType: OptionType
      Value: Money option
      Delta: Money option }

    static member sysRandom = Random()

    static member Random(marketData: MarketData) =
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownCurrenciesDefault = [| "EUR"; "USD"; "PLN" |]

        let knownCurrencies =
            if marketData.ContainsKey "valuation::knownCurrencies" then
                marketData["valuation::knownCurrencies"].Split([| ' ' |])
            else
                knownCurrenciesDefault

        { TradeName = sprintf "EuropeanOption%04d" (EuropeanOptionRecord.sysRandom.Next(9999))
          SpotPrice = EuropeanOptionRecord.sysRandom.Next(50, 500)
          Strike = EuropeanOptionRecord.sysRandom.Next(50, 500)
          Drift = EuropeanOptionRecord.sysRandom.Next(0, 30)
          Volatility = EuropeanOptionRecord.sysRandom.Next(0, 30)
          Expiry = DateTime.Now.AddMonths(EuropeanOptionRecord.sysRandom.Next(1, 6)).Date
          Currency = knownCurrencies[EuropeanOptionRecord.sysRandom.Next(knownCurrencies.Length)]
          ValuationMethod = Analytical
          OptionType = Call
          Value = None
          Delta = None }

(* Complete set of data required for valuation *)
type EuropeanOptionValuationInputs =
    { Trade: EuropeanOptionRecord
      Data: Configuration
      MarketData: MarketData }

type EuropeanOptionValuationModel(inputs: EuropeanOptionValuationInputs) =
    member private this.valuationMethods =
        dict<ValuationMethod * OptionType, float * float>
            [ (Analytical, Call), this.calculateEuropeanOption
              (Analytical, Put), this.calculateEuropeanOption
              (MonteCarlo, Call), this.MonteCarloSimulation
              (MonteCarlo, Put), this.MonteCarloSimulation ]

    member this.drift = inputs.Trade.Drift / 100.0
    member this.volatility = inputs.Trade.Volatility / 100.0
    member this.time = this.calculateTimeToExpiry ()

    member this.PrepareCurrencies() : float * ConfigValue =
        let tradeCcy = inputs.Trade.Currency

        let targetCcy =
            match inputs.MarketData.TryFind "valuation::baseCurrency" with
            | Some ccy -> ccy
            | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate =
            if inputs.Data.ContainsKey fxRateKey then
                float inputs.Data[fxRateKey]
            else
                1.0 // lookup FX rate

        let finalCcy =
            if inputs.Data.ContainsKey fxRateKey then
                targetCcy
            else
                tradeCcy

        (fxRate, finalCcy)

    member private this.calculateTimeToExpiry() : float =
        let today = DateTime.Now
        let difference = inputs.Trade.Expiry - today
        let totalDaysInYear = 365.0
        let partOfYear = difference.TotalDays / totalDaysInYear
        partOfYear

    member private this.calculateD1Formula() : float =
        //print inputs
        printfn $"SpotPrice: %f{inputs.Trade.SpotPrice}"
        printfn $"Strike: %f{inputs.Trade.Strike}"
        printfn $"Drift: %f{this.drift}"
        printfn $"Volatility: %f{this.volatility}"
        printfn $"Time: %f{this.time}"

        let numerator =
            log (inputs.Trade.SpotPrice / inputs.Trade.Strike)
            + (this.drift + 0.5 * this.volatility ** 2.0) * this.time

        let denominator = this.volatility * sqrt this.time
        printf $"D1: %f{numerator / denominator}"
        numerator / denominator


    member private this.calculateEuropeanOption : float * float =
        let d1 = this.calculateD1Formula()
        let d2 = d1 - this.volatility * sqrt this.time
        printfn $"D2: %f{d2}"
        let d1CDF, d2CDF = Normal.CDF(0.0, 1.0, d1), Normal.CDF(0.0, 1.0, d2)
        let exp = exp (-1.0 * this.drift * this.time)

        let payoff =
            match inputs.Trade.OptionType with
            | Call -> inputs.Trade.SpotPrice * d1CDF - inputs.Trade.Strike * exp * d2CDF
            | Put ->
                inputs.Trade.Strike * exp * (1.0 - d2CDF)
                - inputs.Trade.SpotPrice * (1.0 - d1CDF)

        let delta =
            match inputs.Trade.OptionType with
            | Call -> d1CDF
            | Put -> d1CDF - 1.0

        payoff, delta


    member private this.MonteCarloSimulation : float * float =
        let runs = inputs.MarketData["monteCarlo::runs"] |> int
        printfn $"Monte Carlo runs: %d{runs}"
        let normal = Normal(inputs.Trade.SpotPrice, inputs.Trade.Volatility)
        let randomNumbers = Array.init runs (fun _ -> normal.Sample())

        let payoffs =
            randomNumbers
            |> Array.map (fun x -> max (x - inputs.Trade.Strike) 0.0)
            |> Array.map (fun x -> x * exp (-this.drift * this.time))

        let averagePayoff = Array.average payoffs
        let delta = averagePayoff / inputs.Trade.SpotPrice
        averagePayoff, delta




    member this.Calculate() : Money * Money =
        let fxRate, finalCcy = this.PrepareCurrencies()

        let payoff, delta =
            this.valuationMethods.Item ((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))

        { Value = payoff / fxRate
          Currency = finalCcy },
        { Value = delta / fxRate
          Currency = finalCcy }
