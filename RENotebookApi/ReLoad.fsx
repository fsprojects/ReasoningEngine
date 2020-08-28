module ReasoningEngine

#r @".\bin\Release\net472\ReasoningEngine.dll"
#r @".\bin\Release\net472\RENotebookApi.dll"

#load "Paket.fsx"
Paket.Version [ "XPlot.Plotly", "~> 1.4.2"]   
#load "Paket.Generated.Refs.fsx"
#load "XPlot.Plotly.fsx"


open Microsoft.Research.RENotebook

type ReilAPI = Microsoft.Research.RENotebook.REIL
module Cst = Microsoft.Research.ReasoningEngine.Constraint
module Var = Microsoft.Research.ReasoningEngine.Var
type TrajVis = Microsoft.Research.RENotebook.Lib.TrajectoryVisualization
printfn "Loading the Reasoning Engine (RE)..."


Printers.addDisplayPrinter(fun (Lib.HtmlOutput html) ->
   { ContentType = "text/html"; Data = html})

//Printers.addDisplayPrinter(fun (resultOption:Microsoft.Research.REIN.REIN.Problem option) ->
//    let html = 
//        match resultOption with 
//        | Some result -> 
//            //let (Lib.HtmlOutput inner) = ReinAPI.ProblemToHtml result
//            //inner
//            "Solution(s) exists"
//        | None -> "No solutions found"
//    {ContentType = "text/html"; Data = html}
//    )

//Printers.addDisplayPrinter(fun (result:Microsoft.Research.REIN.REIN.Problem) ->
//     let (Lib.HtmlOutput html) = ReinAPI.ProblemToHtml result
//     {ContentType = "text/html"; Data = html}
//     )
     
   

// Lists the required and disallowed interactions in a table
let DrawInteractions required disallowed = 
    if (required = Set.empty) then
        let rStr = ""
        let dStr = disallowed |> Seq.map (fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + "<br>" + b)
        sprintf "<table><tr><th>Required Interactions</th><th>Disallowed Interactions</th></tr><tr><td>%s</td><td>%s</td>" rStr dStr
        |> IfSharp.Kernel.Util.Html
    elif (disallowed = Set.empty) then
        let rStr = required |> Seq.map (fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + "<br>" + b)
        let dStr = ""
        sprintf "<table><tr><th>Required Interactions</th><th>Disallowed Interactions</th></tr><tr><td>%s</td><td>%s</td>" rStr dStr
        |> IfSharp.Kernel.Util.Html  
    else 
        let rStr = required |> Seq.map (fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + "<br>" + b)
        let dStr = disallowed |> Seq.map (fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + "<br>" + b)
        sprintf "<table><tr><th>Required Interactions</th><th>Disallowed Interactions</th></tr><tr><td>%s</td><td>%s</td>" rStr dStr
        |> IfSharp.Kernel.Util.Html