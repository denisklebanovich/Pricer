module Chart

open EuropeanOption
open Radzen.Blazor
open System
open Configuration

type ChartItem = { XValue: float; YValue: float }

type Series =
    { Values: ChartItem[]
      SeriesName: string
      Line: LineType
      Color: string
      Marker: MarkerType
      Smooth: bool
      ShowLabels: bool }

    static member Default =
        { Values = [||]
          SeriesName = "Series1"
          Line = LineType.Solid
          Color = "Black"
          Marker = MarkerType.None
          Smooth = false
          ShowLabels = false }

type ChartData =
    { Series: Series[]
      Title: string }

    static member Default = { Series = [||]; Title = "Title" }

//TODO: remove after we add non-dummy chart
//how to construct a chart:
let mkDummyChart () : ChartData =

    let r = Random()

    let mkDummySeries () : Series =
        let predefinedChartFunctions =
            [| (fun x -> 20.0 * (sin x)); (fun x -> x); (fun x -> x * x) |]

        let multiplier = r.NextDouble()
        let mapFun = predefinedChartFunctions.[r.Next(predefinedChartFunctions.Length)]
        let name = sprintf "Test series %0.2f" multiplier

        //step 1: have a sequence of values (x,y)
        let series =
            seq {
                for i in 1..10 do
                    yield float i, mapFun (multiplier * float i)
            }

        //step 2: map those to ChartItem
        let values =
            series |> Seq.map (fun (x, y) -> { XValue = x; YValue = y }) |> Array.ofSeq

        //step 3: customize the series, change color, name etc
        { Series.Default with
            Values = values
            SeriesName = name }

    //step 4: add or replace series on existing chart
    { ChartData.Default with
        Series = [| mkDummySeries (); mkDummySeries () |]
        Title = "Dummy Demo Chart" }

let mkOptionsValuesChart (options: List<EuropeanOptionValuationInputs>) : ChartData =
    // Define final stock prices
    let finalStockPrices = [ 50.0..2.0..150.0 ]

    // Calculate option values for different stock prices
    let calcValues (optionInputs: EuropeanOptionValuationInputs) =
        let model = EuropeanOptionValuationModel(optionInputs)

        finalStockPrices
        |> List.map (fun s -> fst (model.calculateWithDifferentSpotPrice s))

    // Generate data for the plot
    let series =
        options
        |> List.map (fun optionInputs ->
            // Prepare sequence of (x, y) values
            let valuesSeq = Seq.zip finalStockPrices (calcValues optionInputs)

            // Map those to ChartItem
            let valuesArray =
                valuesSeq |> Seq.map (fun (x, y) -> { XValue = x; YValue = y }) |> Array.ofSeq

            // Customize the series
            { Series.Default with
                Values = valuesArray
                SeriesName = sprintf "Option %s Value" optionInputs.Trade.TradeName })
        |> Array.ofList

    // Add or replace series on existing chart
    { ChartData.Default with
        Series = series
        Title = "Option Value vs Stock Price" }
