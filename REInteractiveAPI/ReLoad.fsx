module ReasoningEngine

#r "nuget:Microsoft.Z3.x64,4.8.4"
#r "nuget:FsLexYacc.Runtime,10.0.0"
#r "nuget:AutomaticGraphLayout,1.1.9"
#r "nuget:AutomaticGraphLayout.Drawing,1.1.9"
#r "nuget:SixLabors.ImageSharp,1.0.1"
#r "nuget:SixLabors.ImageSharp.Drawing,1.0.0-beta0010"
#r "nuget:XPlot.Plotly,3.0.1"
#r "nuget:Microsoft.DotNet.Interactive.Formatting, 1.0.0-beta.23606.2"

#r @"bin/Release/netstandard2.0/ReasoningEngine.dll"
#r @"bin/Release/netstandard2.0/REIN.dll"
#r @"bin/Release/netstandard2.0/REInteractiveAPI.dll"

open Microsoft.Research.RENotebook
open Microsoft.DotNet.Interactive.Formatting

type ReilAPI = Microsoft.Research.RENotebook.REIL
type ReinAPI = Microsoft.Research.RENotebook.REIN
module Cst = Microsoft.Research.ReasoningEngine.Constraint
module Var = Microsoft.Research.ReasoningEngine.Var
type TrajVis = Microsoft.Research.RENotebook.Lib.TrajectoryVisualization
printfn "Loading the Reasoning Engine (RE)..."


Formatter.Register<Lib.PlotlyOutput>(
    mimeType = "text/html",
    formatter = fun plotly (context: FormatContext) ->
        match plotly with
        | Lib.PlotlyOutput.Chart plot -> context.Writer.Write(plot)
        | Lib.PlotlyOutput.Message stuff -> context.Writer.Write(stuff)
        true)



Formatter.Register<Lib.HtmlOutput>(
    mimeType = "text/html",
    formatter = fun htmlOutput (context: FormatContext) ->
        match htmlOutput with Lib.HtmlOutput html -> context.Writer.Write(html)
        true)


// Lists the required and disallowed interactions in a table
let DrawInteractions required disallowed = 
    if (required = Set.empty) then
        let rStr = ""
        let dStr = disallowed |> Seq.map (fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + "<br>" + b)
        sprintf "<table><tr><th>Required Interactions</th><th>Disallowed Interactions</th></tr><tr><td>%s</td><td>%s</td>" rStr dStr
        |> Lib.HtmlOutput
    elif (disallowed = Set.empty) then
        let rStr = required |> Seq.map (fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + "<br>" + b)
        let dStr = ""
        sprintf "<table><tr><th>Required Interactions</th><th>Disallowed Interactions</th></tr><tr><td>%s</td><td>%s</td>" rStr dStr
        |> Lib.HtmlOutput
    else 
        let rStr = required |> Seq.map (fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + "<br>" + b)
        let dStr = disallowed |> Seq.map (fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + "<br>" + b)
        sprintf "<table><tr><th>Required Interactions</th><th>Disallowed Interactions</th></tr><tr><td>%s</td><td>%s</td>" rStr dStr
        |> Lib.HtmlOutput