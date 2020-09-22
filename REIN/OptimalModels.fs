module Microsoft.Research.REIN.OptimalModels

module Expressions = Microsoft.Research.ReasoningEngine.Constraint
module Var = Microsoft.Research.ReasoningEngine.Var
module Optimizer = Microsoft.Research.ReasoningEngine.Optimizer

let InteractionsCountObjective (problem:REIN.Problem) = 
    problem.interactions
    |> Seq.filter(fun i -> not i.definite) //select all optional interactions
    |> Seq.map(fun i -> i.var) //get the name of the variable that represents each interaction
    |> Seq.map(fun v -> Expressions.BTerm(Expressions.BVar(Var.SysVar(v)))) //map each var name to a system variable
    |> Expressions.Card //take the cardinality of all possible interactions
    |> Expressions.NTerm

let FindMinimalModels (problem:REIN.Problem) =         
    problem 
    |> Translation.Translate //convert the problem to REIL
    |> Optimizer.Maximize (InteractionsCountObjective problem) //generate all models where the number of interactions is minimized  






//Find minimum models
//Strategy 1: at each step, generate several models. Pick the smallest of these, fix the interactions that were not found and repeat (not used)
//Strategy 2: at each step, generate a model and ensure that not all interactions are used in the following searches
let FindMinimumModels (problem:REIN.Problem) = 
        
    //find the smallest solution out of the enumerated ones and extract its interactions
    let solutionsExist = ref true
    let p = ref problem

    seq {
        while !solutionsExist do
            let sol = !p |> Procedures.Solve
            
            match sol with
            | Some(s) -> 
                let not_all_interactions = 
                    s 
                    |> Export.SolutionToInteractions   
                    |> Seq.map(fun i -> Expressions.BTerm(Expressions.BVar(Var.SysVar(i.var))))
                    |> Expressions.LOr
                    |> Expressions.Not                    
                             
                solutionsExist := true                                                                
                p := problem.AddConstraint(REIN.Fact.Create(not_all_interactions,None))//update the problem to remove interactions       
                yield s
            | None -> solutionsExist := false
        }




//Deprecated stand-alone function for finding minimal models
//TODO: Implement a binary search for the minimal model?
//NOTE: Maybe, return the optimization and enumeration seq separately? 
//Currently, only the enumeration seq is returned (i.e. the models leading up to the minimal one are skipped)
//let FindMinimalModels (problem:REIN.Problem) =  
//    let solOpt = Procedures.Solve problem //find one solution
//    if solOpt.IsSome then       
//        let solutionsExist = ref true //flag for termination
//        let n = ref (solOpt.Value |> Export.SolutionToInteractions |> Seq.length) //starting number of interactions                     
//        
//        while !solutionsExist do
//            let problem' = {problem with settings = {problem.settings with interaction_limit = Some(!n-1)}}
//            let solOpt' = Procedures.Solve problem' //generate a single solution
//                    
//            if solOpt'.IsSome then                                                                        
//                let sol_interactions = solOpt'.Value |> Export.SolutionToInteractions
//                n := (sol_interactions |> Seq.length) //store the current least number of interactions
//            else
//                solutionsExist := false                                   
//                 
//        //Once the minimal number of interactions is reached, generate all possible solutions
//        let enumeration = 
//            {problem with settings = {problem.settings with interaction_limit = Some(!n)}}               
//            |> Procedures.Enumerate       
//                    
//        enumeration
//    else
//        Array.empty