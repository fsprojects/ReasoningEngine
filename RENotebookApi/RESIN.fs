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
        |> Option.map (fun sol -> Microsoft.Research.RESIN.Export.Decode problem sol)

    static member CheckAndPrint model = 
        let sol = model |> RESIN.Check
        if sol.IsSome then
            "Solution(s) found"
        else
            "No solution(s) found"

    static member Enumerate n problem  =         
        problem 
        |> RESIN.ToReil        
        |> REIL.Enumerate n
        |> Array.map (fun sol -> Microsoft.Research.RESIN.Export.Decode problem sol)
    
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
    
    static member DrawSummary (problems:seq<Microsoft.Research.RESIN.RESIN.Problem>) =    
        let solutions = problems |> Seq.choose (fun p -> p.solution) |> Array.ofSeq
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
                            let istr = sprintf "<td style=\"border: 1px solid black\">%s</td>" (i.ToString())
                            let sstr = 
                                [0..n-1] 
                                |> Seq.map(fun s ->                                     
                                    Cells
                                    |> Seq.map(fun c -> 
                                        if summary.[i].Contains(c,s) then (if i.positive then "<td bgcolor=\"#00FF00\" style=\"border: 1px solid black\"></td>" else "<td bgcolor=\"#FF0000\" style=\"border: 1px solid black\"></td>") else "<td style=\"border: 1px solid black\"></td>")
                                    |> Seq.reduce(fun a b -> a + "\n" + b))
                                |> Seq.reduce(fun a b -> a + "\n" + b)
                            "<tr>" + istr + sstr + "</tr>")
                        |> Seq.reduce(fun a b -> a + "\n" + b)
    
                    let headers1 = 
                        sprintf "<tr><td style=\"border: 1px solid black\">\tInteraction\t</td>%s</tr>" 
                            ([0..n-1]|> Seq.map(fun i -> sprintf "<th align=\"center\" colspan=%i style=\"border: 1px solid black\">%i</td>" ncells i) 
                            |> Seq.reduce(fun a b -> a + "\n" + b))
                    let headers2 = 
                        let cellsStr =
                            ([0..n-1]
                            |> Seq.map(fun i ->                                
                                    Cells 
                                    |> Set.map(fun c -> sprintf "<td style=\"border: 1px solid black\">%s</td>" c) 
                                    |> Set.toSeq 
                                    |> Seq.reduce(fun a b -> a + "\n" + b))
                            |> Seq.reduce(fun a b -> a + "\n" + b))
                        sprintf "<tr><td style=\"border: 1px solid black\"></td style=\"border: 1px solid black\">%s</tr>" cellsStr
                                
                    "<table style=\"border: 3px solid black\">\n" + headers1 + "\n" + headers2 + "\n" + inner + "\n</table>" 
        Lib.HtmlOutput content


    static member IdentifyInteractions (problem:Microsoft.Research.RESIN.RESIN.Problem) = 

        // Get the set of interactions that correspond to each cell
        let overallInteractions = problem.cells |> Seq.map (fun cell -> cell.cname, (cell.interactions |> Set.ofSeq)) |> Map.ofSeq

        // Find one solution and extract the identified interactions to give us a smaller set of interactions to work from
        let sol = problem |> RESIN.Check 

        let solutionInteractions =
            if sol.IsNone then
                failwith "Cannot search for required / disallowed interactions as no consistent models exist."
            sol.Value.solution.Value
            |> Microsoft.Research.RESIN.Export.SolutionToInteractions
            |> Map.map (fun _ cellinteractions -> cellinteractions |> Set.ofSeq)

       
        // Test each found interaction to see if it is required
        let required =
            overallInteractions
            |> Map.map (fun cellname cellinteractions ->
                // Check that the solution has instantiated interactions for each cell, and if not, assume no required interactions for that cell
                let optionalInteractionsInstantiated = solutionInteractions.TryFind cellname

                if optionalInteractionsInstantiated.IsSome then
                    optionalInteractionsInstantiated.Value                    
                    |> Set.filter(fun interaction -> 
                    //Take a definite interaction and make an optional. I don't know why RE:IN doesn't do this. CG
                    let var_name = cellname + "_" + (if interaction.positive then "Pos_" else "Neg_") + interaction.source + "_" + interaction.target
                    let interactionAsOptional =
                        { Microsoft.Research.REIN.REIN.Interaction.source = interaction.source
                        ; Microsoft.Research.REIN.REIN.Interaction.target = interaction.target
                        ; Microsoft.Research.REIN.REIN.Interaction.positive = interaction.positive
                        ; Microsoft.Research.REIN.REIN.Interaction.definite = false
                        ; Microsoft.Research.REIN.REIN.Interaction.var = var_name}
                        
                    let singleton = interactionAsOptional |> Set.singleton
                    let interactions' = Set.difference cellinteractions singleton |> Set.toSeq
                    
                    //This would be better done as set operations
                    let specificcell = problem.cells |> Seq.find (fun cell -> cell.cname = cellname)
                    //Console.WriteLine("Cell name: {0}", cellname)                      
                    let updatedcell = {specificcell with interactions = interactions'}
                    let updatedcells = problem.cells |> Seq.filter (fun cell -> cell.cname <> cellname) |> Seq.append [updatedcell]
                    let problem' = {problem with cells = updatedcells}
                                         
                    (problem' |> RESIN.Check).IsNone  //return whether no solutions were found
                    //(Microsoft.Research.RESIN.Procedures.Solve problem').IsNone  //return whether no solutions were found
                    )
                else
                    Set.empty                    
                 )
   
        // Find all optional interactions in the problem
        let interactions = 
            overallInteractions
            |> Map.map (fun cellname cellinteractions ->
                cellinteractions
                |> Set.filter (fun interaction -> not interaction.definite))
 
        // Find the interactions that were not included in the solution so that you can then check if they are disallowed
        // The code below looks a bit weird, but again we have to cover the case when the solution that we're basing the analysis on 
        // didn't have any optional interactions instantiated, and so if a particular cell isn't found in the solution, we have to pull
        // all of its optional interactions from the problem
        let possiblyDisallowed =
            solutionInteractions
            |> Map.map
                (fun cellname cellinteractions ->

                    let cellIsInTheSolution = solutionInteractions.TryFind(cellname)

                    if cellIsInTheSolution.IsSome then
                        Set.difference interactions.[cellname] cellinteractions  // Only check those that don't appear in solution
                    else
                        interactions.[cellname]  // grab all the possible interactions for that cell from the problem interactions
                )

        // Test each interaction
        let disallowed =
            possiblyDisallowed
            |> Map.map (fun cellname cellinteractions ->
                    cellinteractions
                    |> Set.filter(fun interaction ->
                        //Take an optional interaction and make it definite - to attempt to prune it.
                        let var_name = cellname + "_" + (if interaction.positive then "Pos_" else "Neg_") + interaction.source + "_" + interaction.target
                        let interactionAsOptional =
                            { Microsoft.Research.REIN.REIN.Interaction.source = interaction.source
                            ; Microsoft.Research.REIN.REIN.Interaction.target = interaction.target
                            ; Microsoft.Research.REIN.REIN.Interaction.positive = interaction.positive
                            ; Microsoft.Research.REIN.REIN.Interaction.definite = false
                            ; Microsoft.Research.REIN.REIN.Interaction.var = var_name}

                        let interactions' = Set.difference overallInteractions.[cellname] (Set.singleton interaction) |> Set.toSeq

                        let interactionAsDefinite =
                            { Microsoft.Research.REIN.REIN.Interaction.source = interaction.source
                            ; Microsoft.Research.REIN.REIN.Interaction.target = interaction.target
                            ; Microsoft.Research.REIN.REIN.Interaction.positive = interaction.positive
                            ; Microsoft.Research.REIN.REIN.Interaction.definite = true
                            ; Microsoft.Research.REIN.REIN.Interaction.var = var_name}

                        let interactions'' = Seq.append interactions' [interactionAsDefinite]
                        //let problem' = {problem with interactions = interactions'}
                        let specificcell = problem.cells |> Seq.find (fun cell -> cell.cname = cellname)
                        let updatedcell = {specificcell with interactions = interactions''}
                        let updatedcells = problem.cells |> Seq.filter (fun cell -> cell.cname <> cellname) |> Seq.append [updatedcell]
                        let problem' = {problem with cells = updatedcells}
                    
                        (problem' |> RESIN.Check).IsNone  //return whether no solutions were found                        
                        )
                        )

        // Construct a new model by taking into account the required and disallowed interactions
        let updatedInteractions' =
            required
            |> Map.map (fun cellname requiredcellinterations ->
                let disallowedcellinteractions = if disallowed.ContainsKey cellname then disallowed.[cellname] else Set.empty

                let requiredcellinterationsforremoval = requiredcellinterations |> Set.map (fun interaction ->
                    let var_name = cellname + "_" + (if interaction.positive then "Pos_" else "Neg_") + interaction.source + "_" + interaction.target
                    { Microsoft.Research.REIN.REIN.Interaction.source = interaction.source
                            ; Microsoft.Research.REIN.REIN.Interaction.target = interaction.target
                            ; Microsoft.Research.REIN.REIN.Interaction.positive = interaction.positive
                            ; Microsoft.Research.REIN.REIN.Interaction.definite = false
                            ; Microsoft.Research.REIN.REIN.Interaction.var = var_name}
                    )

                let disallowedforremoval = disallowedcellinteractions |> Set.map (fun interaction ->
                    let var_name = cellname + "_" + (if interaction.positive then "Pos_" else "Neg_") + interaction.source + "_" + interaction.target
                    { Microsoft.Research.REIN.REIN.Interaction.source = interaction.source
                            ; Microsoft.Research.REIN.REIN.Interaction.target = interaction.target
                            ; Microsoft.Research.REIN.REIN.Interaction.positive = interaction.positive
                            ; Microsoft.Research.REIN.REIN.Interaction.definite = false
                            ; Microsoft.Research.REIN.REIN.Interaction.var = var_name}
                    )

                let preservedcellinteractions = Set.difference overallInteractions.[cellname] (Set.union requiredcellinterationsforremoval disallowedforremoval)

                (preservedcellinteractions, requiredcellinterations)
                ||> Seq.fold(fun I (interaction) ->
                        let var_name = cellname + "_" + (if interaction.positive then "Pos_" else "Neg_") + interaction.source + "_" + interaction.target
                        let interactionAsDefinite =
                            { Microsoft.Research.REIN.REIN.Interaction.source = interaction.source
                            ; Microsoft.Research.REIN.REIN.Interaction.target = interaction.target
                            ; Microsoft.Research.REIN.REIN.Interaction.positive = interaction.positive
                            ; Microsoft.Research.REIN.REIN.Interaction.definite = true
                            ; Microsoft.Research.REIN.REIN.Interaction.var = var_name}

                        Set.add interactionAsDefinite I)   
            
            )

        let updatedcells' =

            problem.cells
            |> Seq.map (fun cell -> {cell with interactions = updatedInteractions'.[cell.cname]})

        //return the modified model
        {problem with cells = updatedcells'}, required, disallowed 


    static member DrawInteractions required disallowed= 
        let requiredAsString =
            required
            |> Map.map (fun cell cellinterations -> cell + "<br>" + ((cellinterations |> Seq.map (fun interaction -> interaction.ToString())) |> String.concat ",") + "<br>")
            |> Map.fold (fun acc key v -> acc + "<br>" + v) ""
  
        let disallowedAsString =
            disallowed
            |> Map.map (fun cell cellinterations -> cell + "<br>" + ((cellinterations |> Seq.map (fun interaction -> interaction.ToString())) |> String.concat ",") + "<br>")
            |> Map.fold (fun acc key v -> acc + "<br>" + v) ""
    
        sprintf "<table><tr><th>Required Interactions</th><th>Disallowed Interactions</th></tr><tr><td>%s</td><td>%s</td></tr></table>" requiredAsString disallowedAsString
        |> Lib.HtmlOutput


    static member CheckMultipleFiles directory = 
        let dir = new System.IO.DirectoryInfo(directory)
        
        dir.GetFiles("*", System.IO.SearchOption.AllDirectories)
        |> Seq.map(fun file -> 
            let hasSol = (file.FullName |> RESIN.LoadFile |> RESIN.Check).IsSome
            sprintf "<tr><td>%s</td><td>%A</td></tr>" file.Name hasSol
            )
        |> String.concat ""
        |> sprintf "<table>%s</table>"
        |> Lib.HtmlOutput