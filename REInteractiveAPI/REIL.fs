namespace Microsoft.Research.RENotebook

//methods that manipulate REIL structures
type REIL = 
    static member Load problemString = 
        problemString |>  Microsoft.Research.ReasoningEngine.Model.Parse

    static member LoadFile (problemFilePath:string) = 
        let problemString = System.IO.File.ReadAllText problemFilePath
        Microsoft.Research.ReasoningEngine.Model.Parse(problemString)

    static member Check problem  = 
        Microsoft.Research.ReasoningEngine.Solver.BMC problem  
    
    static member CheckAndPrint model = 
        let sol = model |> REIL.Check
        if sol.IsSome then
            "Solution(s) found"
        else
            "No solution(s) found"

    static member Enumerate n problem  = 
        Microsoft.Research.ReasoningEngine.Solver.BMCIterN n problem              

    //Experimental methods
    static member Infer problem  = 
        Microsoft.Research.ReasoningEngine.Inference.BMCInference problem  

    //TODO: depends on functions inside REIN.Export that need to be generalized
    static member ObservationsToHtml problem =                 
        let model' = problem |> Microsoft.Research.ReasoningEngine.Tactics.InlinePredicates        
    
        let experiments, variables, values, fpvalues = 
            model'.constraints.observations
            |> Seq.map(fun e -> fst e)
            |> Microsoft.Research.REIN.Export.ExportSimpleExpression
            |> Microsoft.Research.REIN.Export.SummarizeSpec

        let timesteps = 
            experiments 
            |> Seq.map(fun e -> e.Split([|'[';']'|]).[1])

        let experiments = 
            experiments 
            |> Seq.map(fun e -> e.Split('[').[0])

        let headers = 
            let exp_header = 
                experiments
                |> Seq.mapi(fun i e -> 
                    let exp = e |> Seq.map(fun c -> (string)c) |> Seq.reduce(fun a b -> a + "<br/>" + b) 
                    let fp = if fpvalues.[i] = 1 then "*" else if fpvalues.[i] = -1 then "**" else ""
                    sprintf "<td>%s%s</td>" exp fp)
                |> Seq.reduce(fun a b -> a + "\n" + b)
            let step_header = 
                timesteps 
                |> Seq.map(fun t -> sprintf "<td>%s</td>" t)
                |> Seq.reduce(fun a b -> a + "\n" + b)

            "<tr><td></td>" + exp_header + "</tr><tr><td>step</td>" + step_header + "</tr>"
         
        let inner = 
            variables
            |> Seq.mapi(fun i v -> 
                let row = 
                    experiments
                    |> Seq.mapi(fun j e -> sprintf "<td %s></td>"((if values.[i,j]=1 then "bgcolor=\"#0000FF\"" else if values.[i,j]=0 then "bgcolor=\"#999999\"" else "")))
                    |> Seq.reduce(fun a b -> a + "\n" + b)
                sprintf "<tr><td>%s</td>%s</tr>" v row)
            |> Seq.reduce(fun a b -> a + "\n" + b)
            
        Lib.HtmlOutput (sprintf "<table>%s%s</table" headers inner)