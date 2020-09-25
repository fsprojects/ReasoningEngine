module ReasoningEngine

#r "nuget:Microsoft.Z3.x64,4.8.4"
#r "nuget:FsLexYacc.Runtime,10.0.0"
#r "nuget:AutomaticGraphLayout,1.1.9"
#r "nuget:AutomaticGraphLayout.Drawing,1.1.9"
#r "nuget:SixLabors.ImageSharp,1.0.1"
#r "nuget:SixLabors.ImageSharp.Drawing,1.0.0-beta0010"

#r @"bin/Release/netstandard2.0/ReasoningEngine.dll"
#r @"bin/Release/netstandard2.0/REIN.dll"
#r @"bin/Release/netstandard2.0/REInteractiveAPI.dll"

open Microsoft.Research.RENotebook

type ReilAPI = Microsoft.Research.RENotebook.REIL
type ReinAPI = Microsoft.Research.RENotebook.REIN
module Cst = Microsoft.Research.ReasoningEngine.Constraint
module Var = Microsoft.Research.ReasoningEngine.Var
type TrajVis = Microsoft.Research.RENotebook.Lib.TrajectoryVisualization
printfn "Loading the Reasoning Engine (RE)..."

Formatter.Register<Microsoft.Research.RENotebook.Lib.PlotlyOutput>(
    mimeType = "text/html",
    formatter = Func<_,_,_,_>(fun context plotly (writer: TextWriter) ->
        match plotly with
        | Microsoft.Research.RENotebook.Lib.PlotlyOutput.Chart plot -> display(plot) |> ignore
        | Microsoft.Research.RENotebook.Lib.PlotlyOutput.Message stuff -> display(stuff) |> ignore
        true))

Formatter.Register<Microsoft.Research.RENotebook.Lib.HtmlOutput>(
    mimeType = "text/html",
    formatter = Func<_,_,_,_>(fun context htmlOutput (writer: TextWriter) ->
        match htmlOutput with Microsoft.Research.RENotebook.Lib.HtmlOutput html -> display(HTML(html)) |> ignore
        true))