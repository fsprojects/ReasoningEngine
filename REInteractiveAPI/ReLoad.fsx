module ReasoningEngine

#r @"./bin/Release/netstandard2.0/ReasoningEngine.dll"
#r @"./bin/Release/netstandard2.0/REInteractiveApi.dll"

open Microsoft.Research.RENotebook

type ReilAPI = Microsoft.Research.RENotebook.REIL
module Cst = Microsoft.Research.ReasoningEngine.Constraint
module Var = Microsoft.Research.ReasoningEngine.Var
type TrajVis = Microsoft.Research.RENotebook.Lib.TrajectoryVisualization
printfn "Loading the Reasoning Engine (RE)..."

Formatter.Register<Microsoft.Research.RENotebook.Lib.PlotlyOutput>(
    mimeType = "text/html",
    formatter = Func<_,_,_,_>(fun context person (writer: TextWriter) ->
        match person with
        | Microsoft.Research.RENotebook.Lib.PlotlyOutput.Chart plot -> display(plot) |> ignore
        | Microsoft.Research.RENotebook.Lib.PlotlyOutput.Message stuff -> display(stuff) |> ignore
        true))