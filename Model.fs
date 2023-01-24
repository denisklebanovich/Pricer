module Model
open System
open Bolero
open Configuration
open Trades
open Chart

//regular trade with UI specific metadata
//we need unique id to identify trades on UI
type UITrade = 
     {
          trade : Trade
          id : Guid
     }
     member this.Name =
          match this.trade with
          | Payment p -> p.TradeName

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/config">] Config

/// The Elmish application's model.
type Model =
    {
        page: Page
        trades : Map<Guid,UITrade>
        marketData: MarketData
        configuration : Configuration
        chart : ChartData
        error: string option
    }

    static member Default = 
      {
          page = Home
          trades = Map.empty
          marketData = Map.empty
          configuration = Map.empty
          chart = mkDummyChart () //TODO: change to some actual data
          error = None
      }

module Trades =
  let wrap t  =
      { trade = t; id = Guid.NewGuid() }

  let map f t = { t with trade = f t.trade }

  let tryMap f t =
      match f t.trade with
      | Some t' -> Some { t with trade = t' }
      | None -> None

  let choose picker (trades : Map<_,UITrade>) : 'a list=
      trades 
      |> Map.values 
      |> List.ofSeq
      |> List.choose picker

  let onlyPayments (trades : Map<_,UITrade>) =
      trades |> choose (fun t -> match t.trade with 
                                  | Payment p -> Some <| (t.id,p)
                                  // | _ -> None //this line will be needed when there's more trade types
                        )