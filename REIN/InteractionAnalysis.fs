module Microsoft.Research.REIN.InteractionAnalysis





//Deprecated stand-alone function for finding required interactions
let FindRequiredInteractions (problem:REIN.Problem) =  

    //get the interactions from the problem as a set    
    let problemInteractions = problem.interactions |> Set.ofSeq

    //find one solution and extract the identified interactions as a set
    let solutionInteractions = 
        match (Procedures.Solve problem) with
            | None -> failwith "Cannot search for required interactions since no consistent models exist"
            | Some(sol) ->  sol |> Export.SolutionToInteractions |> Set.ofSeq

    //test each found interaction to see if it is required
    let required = 
        Set.filter(fun i -> 
            let singleton = [i] |> Set.ofList
            let interactions' = Set.difference problemInteractions singleton |> Set.toSeq
            let problem' = {problem with interactions = interactions'}
            
            (Procedures.Solve problem').IsNone  //return whether no solutions were found
            ) solutionInteractions
 
    //test each non-included interaction to see if it is disallowed
    //find all optional interactions in the problem
    let interactions = 
        problem.interactions 
        |> Seq.filter(fun i -> not i.definite)
        |> Set.ofSeq
 
    //find the interactions that were not included in the solution
    let possiblyDisallowed = Set.difference interactions solutionInteractions

    //test each found interaction to see if it is required
    let disallowed = 
        Set.filter(fun i -> 
            
            let interactions' = Set.difference problemInteractions (Set.singleton(i)) |> Set.toSeq
            let problem' = {problem with interactions = interactions'}
            let problem' = problem'.AddInteraction(REIN.Interaction.Create(i.source,i.target,i.positive,true)) //add the interaction as definite
            (Procedures.Solve problem').IsNone //return whether no solutions were found
            ) possiblyDisallowed        
       
    //construct a new model by taking into account the required and disallowed interactions
    let interactions' = 
        //remove the required and disallowed interactions
        let preserved = Set.difference (problem.interactions |> Set.ofSeq) (Set.union required disallowed)         
        Set.fold(fun I (i:REIN.Interaction) -> 
            let i' = REIN.Interaction.Create(i.source,i.target,i.positive,true) //definite interaction
            Set.add i' I) preserved required                   

    //return the modified model
    {problem with interactions = interactions'}, required, disallowed







//let FindRequiredInteractions model spec =     
//    //translate model and spec to REIL
//    let modelREIL = Microsoft.Research.REIN.Translation.Translate model
//    let specREIL = Microsoft.Research.Constraints.Main.Translate spec
//
//    //merge model and spec to create the problem
//    let problem = Microsoft.Research.ReasoningEngine.Combinator.MergeModels modelREIL specREIL
//
//    //find a single solution to reduce the search
//    let sol = Microsoft.Research.ReasoningEngine.Solver.BMC problem       
//
//    //find required interactions
//    let interaction_hypotheses = generateInteractionHypothesesFromSolution model sol
//    let interaction_status = interaction_hypotheses |> Seq.map (fun h -> testHypothesis (problem, h))
//
//    //output results
//    interaction_status
//    |> Seq.map(fun h -> h,h.Status())
//    |> Seq.filter(fun (h, hs) -> 
//        match hs with
//        | True
//        | False -> true
//        | _ -> false)
//    |> Seq.iter(fun (h,_) -> printfn "%s: %s" h.name (h.ToString()))
//