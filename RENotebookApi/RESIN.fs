namespace Microsoft.Research.RENotebook

     
type RESIN = 
//IO methods
    static member Load (problemString:string) = 
        Microsoft.Research.RESIN.RESIN.ParseAST(problemString)

    static member LoadFile (problemFile:string) = 
        let problemStream = (new System.IO.StreamReader(problemFile))
        let problemString = problemStream.ReadToEnd() 
        Microsoft.Research.RESIN.RESIN.ParseAST(problemString)

    static member ToReil problem = 
        Microsoft.Research.RESIN.RESIN.Translate(problem)
    
    //Solver methods
    static member Check problem = 
        problem
        |> RESIN.ToReil
        |> REIL.Check

    static member Enumerate n problem  =         
        problem 
        |> RESIN.ToReil        
        |> REIL.Enumerate n

    //Visualization methods
    static member ObservationsToHtml (model:Microsoft.Research.RESIN.RESIN.Problem) = 
        model
        |> RESIN.ToReil
        //|> Microsoft.Research.ReasoningEngine.Tactics.InlineFixpoint
        |> Microsoft.Research.ReasoningEngine.Tactics.InlinePredicates
        |> REIL.ObservationsToHtml
    
    static member DrawObservations model =
        model
        |> RESIN.ObservationsToHtml              
       

    static member DrawNetworkWithSize width model =                                         
        let networks = 
            model
            |> Microsoft.Research.RESIN.Export.CellsToRein
            |> Seq.map(fun cell -> 
                cell
                |> Lib.REIN.ModelToGraph
                |> Lib.MSAGL.GraphToSvg width                   
                )
            |> Seq.map(fun cellSvg -> sprintf "<td>%s</td>" cellSvg)
            |> Seq.reduce(fun a b -> a + "\n" + b)
        
//        let headers = 
//            Array.init (Seq.length model.cells) (fun i -> sprintf "<th align=\"center\">Cell %i</th>" i)
//            |> Seq.reduce(fun a b -> a + "\n" + b)

        let headers = 
            model.cells
            |> Seq.map (fun c -> c.cname.TrimStart([|'&'|]) |> sprintf "<th align=\"center\">%s</th>")
            |> Seq.reduce(fun a b -> a + "\n" + b)

        sprintf "<table><tr>%s</tr><tr>%s</tr>" headers networks
        |> Lib.HtmlOutput
        
    static member DrawNetwork model =        
        RESIN.DrawNetworkWithSize 640.0 model         



    static member DrawSwitchesWithSize width model =                                         
        model 
        |> Lib.RESIN.SwitchesToGraph
        |> Lib.Jupyter.ShowGraphSvg width

    static member DrawSwitches model =        
        RESIN.DrawSwitchesWithSize 640.0 model
    
    static member DrawSummary solutions =     
        let solutions = solutions |> Array.ofSeq
        let n = Seq.length solutions
        let summary = Microsoft.Research.RESIN.Export.Summarize solutions
            
        let content = 
            if n = 0 then "No solutions found" 
            else
                //get all cell types
                let Cells = 
                    summary.Keys
                    |> Seq.map(fun i -> summary.[i] |> Set.map fst)
                    |> Set.ofSeq
                    |> Seq.reduce (fun a b -> Set.union a b)
                let ncells = Cells.Count

                if Seq.isEmpty summary.Keys then
                    sprintf "%i solutions found but there are no interactions to display" n
                else
                    let inner = 
                        summary.Keys
                        |> Seq.map(fun i -> 
                            let istr = sprintf "<td>%s</td>" (i.ToString())
                            let sstr = 
                                [0..n-1] 
                                |> Seq.map(fun s ->                                     
                                    Cells
                                    |> Seq.map(fun c -> 
                                        if summary.[i].Contains(c,s) then (if i.positive then "<td bgcolor=\"#00FF00\"></td>" else "<td bgcolor=\"#FF0000\"></td>") else "<td></td>")
                                    |> Seq.reduce(fun a b -> a + "\n" + b))
                                |> Seq.reduce(fun a b -> a + "\n" + b)
                            "<tr>" + istr + sstr + "</tr>")
                        |> Seq.reduce(fun a b -> a + "\n" + b)
    
                    let headers1 = 
                        sprintf "<tr><td>\tInteraction\t</td>%s</tr>" 
                            ([0..n-1]|> Seq.map(fun i -> sprintf "<th align=\"center\" colspan=%i>%i</td>" ncells i) 
                            |> Seq.reduce(fun a b -> a + "\n" + b))
                    let headers2 = 
                        let cellsStr =
                            ([0..n-1]
                            |> Seq.map(fun i ->                                
                                    Cells 
                                    |> Set.map(fun c -> sprintf "<td>%s</td>" c) 
                                    |> Set.toSeq 
                                    |> Seq.reduce(fun a b -> a + "\n" + b))
                            |> Seq.reduce(fun a b -> a + "\n" + b))
                        sprintf "<tr><td></td>%s</tr>" cellsStr
                                
                    "<table>\n" + headers1 + "\n" + headers2 + "\n" + inner + "\n</table>" 
        Lib.HtmlOutput content