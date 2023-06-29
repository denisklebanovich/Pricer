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
    | Binomial

let parseValuationMethod (input: string) =
    match input with
    | "Analytical" -> Some Analytical
    | "MonteCarlo" -> Some MonteCarlo
    | "Binomial" -> Some Binomial
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
          SpotPrice = EuropeanOptionRecord.sysRandom.Next(50, 200)
          Strike = EuropeanOptionRecord.sysRandom.Next(50, 200)
          Drift = EuropeanOptionRecord.sysRandom.Next(0, 10)
          Volatility = EuropeanOptionRecord.sysRandom.Next(0, 30)
          Expiry = DateTime.Now.AddMonths(EuropeanOptionRecord.sysRandom.Next(1, 12)).Date
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
        let numerator =
            log (inputs.Trade.SpotPrice / inputs.Trade.Strike)
            + (this.drift + 0.5 * this.volatility ** 2.0) * this.time

        let denominator = this.volatility * sqrt this.time
        numerator / denominator


    member private this.BlackScholesSimulation() : float * float =
        let d1 = this.calculateD1Formula()
        let d2 = d1 - this.volatility * sqrt this.time
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
    
    member public this.calculateWithDifferentSpotPrice(spotPrice: float) : float * float =
        let newTrade = { inputs.Trade with SpotPrice = spotPrice }
        let newInputs = { inputs with Trade = newTrade }
        let model = EuropeanOptionValuationModel(newInputs)
        model.BlackScholesSimulation()
    member private this.MonteCarloSimulation() : float * float =
        let runs = inputs.MarketData["monteCarlo::runs"] |> int
        let normal = Normal(0.0, 1.0)
        let randomNumbers = Array.init runs (fun _ -> normal.Sample())
        let bump = inputs.MarketData["methodology::bumpSize"] |> float

        let calculateAveragePayoff (spotPrice: float) : float =
            let mutable totalPayoff = 0.0
            for i in 0..runs - 1 do
                let stochasticTerm = this.drift * this.time + this.volatility * sqrt(this.time) * randomNumbers[i]
                let stockPrice = spotPrice * exp(stochasticTerm)

                let payoff =
                    match inputs.Trade.OptionType with
                    | Call -> max (stockPrice - inputs.Trade.Strike) 0.0
                    | Put -> max (inputs.Trade.Strike - stockPrice) 0.0

                let discountedPayoff = payoff * exp(-this.drift * this.time)
                totalPayoff <- totalPayoff + discountedPayoff
            totalPayoff / float runs

        let spotPrice = inputs.Trade.SpotPrice
        let averagePayoff = calculateAveragePayoff spotPrice
        let averagePayoffBumpedUp = calculateAveragePayoff (spotPrice + bump)
        let averagePayoffBumpedDown = calculateAveragePayoff (spotPrice - bump)
        let delta = (averagePayoffBumpedUp - averagePayoffBumpedDown) / (2.0 * bump)

        averagePayoff, delta
    
    member private this.BinomialTreeSimulation() : float * float =
        let steps = inputs.MarketData["monteCarlo::runs"] |> int
        let dt = this.time / float steps
        let u = exp(this.volatility * sqrt dt)
        let d = 1.0 / u
        let p = (exp(this.drift * dt) - d) / (u - d)
        let disc = exp(-this.drift * dt)
        let bump = inputs.MarketData["methodology::bumpSize"] |> float

        let createOptionTree (spotPrice: float) =
            let stockTree = Array2D.zeroCreate (steps + 1) (steps + 1)
            for i in 0..steps do
                for j in 0..i do
                    stockTree[i, j] <- spotPrice * Math.Pow(u, float j) * Math.Pow(d, float (i - j))

            let optionTree = Array2D.zeroCreate (steps + 1) (steps + 1)
            for j in 0..steps do
                let stockPrice = stockTree[steps, j]
                optionTree[steps, j] <- 
                    match inputs.Trade.OptionType with
                    | Call -> max (stockPrice - inputs.Trade.Strike) 0.0
                    | Put -> max (inputs.Trade.Strike - stockPrice) 0.0

            for i in [steps - 1.. -1 ..0] do
                for j in 0..i do
                    let expectedValue = disc * (p * optionTree[i + 1, j + 1] + (1.0 - p) * optionTree[i + 1, j])
                    optionTree[i, j] <- expectedValue

            optionTree[0, 0]

        let optionPrice = createOptionTree inputs.Trade.SpotPrice

        // Delta calculation
        let upPrice = inputs.Trade.SpotPrice * (1.0 + bump)
        let downPrice = inputs.Trade.SpotPrice * (1.0 - bump)
        let upOptionPrice = createOptionTree upPrice
        let downOptionPrice = createOptionTree downPrice
        let delta = (upOptionPrice - downOptionPrice) / (2.0 * bump * inputs.Trade.SpotPrice)

        optionPrice, delta




    member this.Calculate() : Money * Money =
        let fxRate, finalCcy = this.PrepareCurrencies()

        let payoff, delta = match inputs.Trade.ValuationMethod with
                            | Analytical -> this.BlackScholesSimulation()
                            | MonteCarlo -> this.MonteCarloSimulation()
                            | Binomial -> this.BinomialTreeSimulation()

        { Value = payoff / fxRate
          Currency = finalCcy },
        { Value = delta / fxRate
          Currency = finalCcy }
