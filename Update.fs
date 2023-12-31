module Update

open Configuration
open Elmish
open Messages
open Model
open Payment
open EuropeanOption
open System
open System.Net.Http
open System.Net.Http.Json
open Trades
open Valuation

let changeTrade (trades : Map<TradeID,UITrade>) id f =
        match Map.tryFind id trades with
        | Some t -> 
            match f t with
            | Some t' -> Map.add id t' trades, Cmd.none
            | None -> trades, Cmd.ofMsg <| Warning (sprintf "could not update trade %s (%A)" t.Name id)
        | None -> trades, Cmd.ofMsg <| Warning (sprintf "could not find trade %A" id)

let tradeChangeUpdate (model : Model) = function
    | NewName (id,name) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> Some <| Payment { p with TradeName = name}
                                | EuropeanOption eo -> Some <| EuropeanOption { eo with TradeName = name}
                            )
            )

    | NewPrincipal (id,principal) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> 
                                    Int64.TryParse(principal)
                                    |> Utils.ofBool
                                    |> Option.map (fun principal ->
                                            Payment { p with Principal = principal})
                                | _ -> None
                            )
            )

    | NewExpiry (id,expiry) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> 
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                            Payment { p with Expiry = expiry})
                                | EuropeanOption eo -> 
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                            EuropeanOption { eo with Expiry = expiry})
                            )
            )

    | NewCurrency (id,ccy) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> Some <| Payment { p with Currency = ccy}
                                | EuropeanOption eo -> Some <| EuropeanOption { eo with Currency = ccy}))

    | NewSpotPrice (id,spot) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(spot)
                                    |> Utils.ofBool
                                    |> Option.map (fun spot ->
                                            EuropeanOption { eo with SpotPrice = spot})
                                | _ -> None))

    | NewStrike (id,strike) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(strike)
                                    |> Utils.ofBool
                                    |> Option.map (fun strike ->
                                            EuropeanOption { eo with Strike = strike})
                                | _ -> None))

    | NewDrift (id,drift) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(drift)
                                    |> Utils.ofBool
                                    |> Option.map (fun drift ->
                                            EuropeanOption { eo with Drift = drift})
                                | _ -> None))

    | NewVolatility (id,volatility) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(volatility)
                                    |> Utils.ofBool
                                    |> Option.map (fun volatility ->
                                            EuropeanOption { eo with Volatility = volatility})
                                | _ -> None))

    | NewValuationMethod (id,valuationMethod) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo ->
                                    parseValuationMethod valuationMethod
                                    |> Option.map (fun valuationMethod ->
                                            EuropeanOption { eo with ValuationMethod = valuationMethod})
                                | _ -> None))
    | NewOptionType (id,optionType) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo ->
                                    parseOptionType optionType
                                    |> Option.map (fun optionType ->
                                            EuropeanOption { eo with OptionType = optionType})
                                | _ -> None))
                                    

let update (http: HttpClient) message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | AddPayment ->
        let newPayment = Trades.wrap (Payment <| PaymentRecord.Random(model.configuration))
        let newTrades = Map.add newPayment.id newPayment model.trades
        { model with trades = newTrades }, Cmd.none
    | AddEuropeanOption ->
        let newOption = Trades.wrap (EuropeanOption <| EuropeanOptionRecord.Random(model.configuration))
        let newTrades = Map.add newOption.id newOption model.trades
        { model with trades = newTrades }, Cmd.none
    | RemoveTrade(tradeId) ->
        let newTrades = Map.remove tradeId model.trades
        { model with trades = newTrades }, Cmd.none
    | TradeChange msg ->
        let newTrades,cmd = tradeChangeUpdate model msg
        { model with trades = newTrades }, Cmd.batch [cmd; Cmd.ofMsg RecalculateAll] 
    | ConfigChange (key,value) ->
        let config = model.configuration
        let config' = Map.add key value config
        { model with configuration = config'}, Cmd.none
    | MarketDataChange (key,value) ->
        let md = model.marketData
        let md' = Map.add key value md
        { model with marketData = md'}, Cmd.ofMsg RecalculateAll
    | GotMarketData response ->
        let c = response |> 
                Array.collect (fun cat -> 
                        cat.Config 
                        |> Array.map (fun {Key = k; Value = v} ->
                            sprintf "%s::%s" cat.Category k, v))
                |> Map.ofArray
        { model with marketData = c }, Cmd.none
    | LoadData ->
        let getConfig() = http.GetFromJsonAsync<JsonConfig>("/configuration.json")
        let conf = Cmd.OfTask.either getConfig () GotConfig Error
        let getMDConfig() = http.GetFromJsonAsync<JsonConfig>("/marketDataConfig.json")
        let mdConf = Cmd.OfTask.either getMDConfig () GotMarketData Error
        { model with configuration = Map.empty }, Cmd.batch [conf; mdConf]
    | GotConfig response -> 
        let c = response |> 
                Array.collect (fun cat -> 
                        cat.Config 
                        |> Array.map (fun {Key = k; Value = v} ->
                            sprintf "%s::%s" cat.Category k, v))
                |> Map.ofArray
        { model with configuration = c }, Cmd.none
    | RecalculateAll ->
        let trades =
             model.trades
             |> Map.map (fun _ -> Trades.map <| valuateTrade model.marketData model.configuration)
        { model with trades = trades }, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | Warning err ->
        { model with error = Some err }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
