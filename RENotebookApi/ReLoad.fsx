module ReasoningEngine

#r @".\bin\Release\net472\ReasoningEngine.dll"
#r @".\bin\Release\net472\REIN.dll"
#r @".\bin\Release\net472\RESIN.dll"
#r @".\bin\Release\net472\ReinMoCo.dll"
#r @".\bin\Release\net472\RENotebookApi.dll"

open Microsoft.Research.RENotebook

type ReilAPI = Microsoft.Research.RENotebook.REIL
type ReinAPI = Microsoft.Research.RENotebook.REIN
type ResinAPI = Microsoft.Research.RENotebook.RESIN
type MotifsAPI = Microsoft.Research.RENotebook.ReinMoCo
module Cst = Microsoft.Research.ReasoningEngine.Constraint
module Var = Microsoft.Research.ReasoningEngine.Var
type TrajVis = Microsoft.Research.RENotebook.Lib.TrajectoryVisualization
printfn "Loading the Reasoning Engine (RE)..."


Printers.addDisplayPrinter(fun (Lib.HtmlOutput html) ->
   { ContentType = "text/html"; Data = html})

Printers.addDisplayPrinter(fun (resultOption:Microsoft.Research.REIN.REIN.Problem option) ->
    let html = 
        match resultOption with 
        | Some result -> 
            //let (Lib.HtmlOutput inner) = ReinAPI.ProblemToHtml result
            //inner
            "Solution(s) exists"
        | None -> "No solutions found"
    {ContentType = "text/html"; Data = html}
    )

//Printers.addDisplayPrinter(fun (result:Microsoft.Research.REIN.REIN.Problem) ->
//     let (Lib.HtmlOutput html) = ReinAPI.ProblemToHtml result
//     {ContentType = "text/html"; Data = html}
//     )
     
