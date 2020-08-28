module Microsoft.Research.ReasoningEngine.Solver
open Model
open Solution



//Basic Z3 solver: returns a single solution
let Z3Solve (z3:Microsoft.Z3.Context, (constraints, variables:Map<Var.Var,Microsoft.Z3.Expr>)) = 
    let sol = z3.MkSolver()
    sol.Assert([|constraints|]);
    if (sol.Check()=Microsoft.Z3.Status.SATISFIABLE) then
        let res = Seq.map(fun ((v:Var.Var),e) -> v,sol.Model.Eval(e,true).ToString()) (variables |>Map.toSeq) |> Map.ofSeq in        
        Some(Solution.Construct(res))        
    else
        None

//Iterative Z3 solver: returns a set of solutions
let Z3SolveIter (use_unique:bool) limit (V: Map<string,Var.VarDef>) (z3:Microsoft.Z3.Context, (constraints, variables:Map<Var.Var,Microsoft.Z3.Expr>)) = 
    let sol = z3.MkSolver()
    sol.Assert([|constraints|]);  
        

    let unique_variables = 
        if use_unique then 
            variables |> Map.filter(fun v _ -> V.[v.Name].unique)
        else
            variables


    let cnt = ref 0
    seq {
        while (!cnt < limit && sol.Check()=Microsoft.Z3.Status.SATISFIABLE) do
            let values = Map.map(fun v e -> sol.Model.Eval(e,true)) variables in            
            let res = Map.map(fun n e -> e.ToString()) values in                                     
            yield(Solution.Construct(res))   
            
            //increment the number of solutions
            cnt := !cnt + 1

            //assert uniqueness constraint
            unique_variables
            |> Map.map(fun v e -> z3.MkNot(z3.MkEq(variables.[v],values.[v])))
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.toArray
            |> fun e -> z3.MkOr(e)                        
            |> fun e -> sol.Assert(e)
        } |> Seq.toArray


//Unsat core Z3 solver.
//NOTE: this functionality is still under development
let Z3SolveCore (z3:Microsoft.Z3.Context) (choice_vars, constraints, (variables:Map<Var.Var,Microsoft.Z3.Expr>)) = 
    let sol = z3.MkSolver() in
    sol.Assert([|constraints|]);
    let assumptions = Seq.map(fun e -> e:>Microsoft.Z3.Expr) choice_vars |> Array.ofSeq in
    if (sol.Check(assumptions)=Microsoft.Z3.Status.SATISFIABLE) then
        let res = Seq.map(fun ((v:Var.Var),e) ->             
            v,sol.Model.Eval(e,true).ToString()) (variables |>Map.toSeq) |> Map.ofSeq in
        Some(Solution.Construct(res))        
    else
        let core = sol.UnsatCore in
        Seq.iter(fun (e:Microsoft.Z3.BoolExpr) -> printfn "%s" (e.ToString())) core
        None




//let time name f A = 
//    let t = System.DateTime.Now
//    let result = f A
//    printfn "%s:\t %.2f seconds" name ((System.DateTime.Now-t).TotalSeconds)
//    result
//
////pre-processing procedures for BMC
//let BMCPreProcess (model:Model) =                
//    model
//    |> time " Type checks\t\t" Checks.TypeCheck   //perform basic sanity checkes (TODO: is this the right place for such functionality?)              
//    |> time " Inlining\t\t" Tactics.InlinePredicates         //TODO: PREDICATES ARE INLINED AS PART OF THE Z3 ENCODING....IS THIS NEEDED!?
//    |> time " Dynamics to Constraints" Tactics.DynamicsToConstraints
//    |> time " Generating type bounds\t" Tactics.GenerateTypeBounds    
//
//let BMC (model:Model) =                   
//    model
//    |> time "PreProcessing" BMCPreProcess            //pre-process
//    |> time "Encoding" Encodings.Encoding.Encode    //encode
//    |> time "Solving" Z3Solve                      //solve (single solution)


//pre-processing procedures for BMC
let BMCPreProcess (model:Model) =                
    model
    |> Checks.TypeCheck   //perform basic sanity checkes (TODO: is this the right place for such functionality?)        
    |> Tactics.InlineFixpoint //replace fixpoint constraints with expressions      
    |> Tactics.InlinePredicates  //TODO: PREDICATES ARE INLINED AS PART OF THE Z3 ENCODING....IS THIS NEEDED!?
    |> Tactics.DynamicsToConstraints //unroll trajectories and convert update rules to expressions
    |> Tactics.GenerateTypeBounds    //assert upper and lower bounds for the appropriate variable types

let BMC (model:Model) =                       
    model
    |> BMCPreProcess                //pre-process
    |> Encodings.Z3Encoding<_>.Encode    //encode
    |> Z3Solve                      //solve (single solution)






let BMCIter (model:Model) =                  
    model
    |> BMCPreProcess                    //pre-process
    |> Encodings.Z3Encoding<_>.Encode        //encode
    |> Z3SolveIter true model.settings.enumerate model.system.varDefs //solve (multiple solutions, using the unique variables definitions)


let BMCIterN n (model:Model) =                  
    model
    |> BMCPreProcess                    //pre-process
    |> Encodings.Z3Encoding<_>.Encode        //encode
    |> Z3SolveIter true n model.system.varDefs //solve (multiple solutions, using the unique variables definitions)


let AllVarsBMCIterN n (model:Model) =                  
    model
    |> BMCPreProcess                    //pre-process
    |> Encodings.Z3Encoding<_>.Encode        //encode
    |> Z3SolveIter false n model.system.varDefs //solve (multiple solutions, using the unique variables definitions)




let BMCMaxSat (model:Model) (e:Constraint.NExpr) (limit: int option)=            
    let model = model |> BMCPreProcess           
    match model.settings.encoding with
        | Settings.Integer -> 
            let encoding = Encodings.Z3Encoding<_>.MkEncoding (Encodings.Z3Encoding<_>.MkBoolIntEncoding) model            
            let cst, vars = encoding.Encode(model)
            let z3 = encoding.ctx

            //a sequence of solutions, along which the optimization function is decreased
            let optimization_solutions =                               
                let sol = z3.MkSolver()
                sol.Assert(cst)        
                seq {
                    while (sol.Check()=Microsoft.Z3.Status.SATISFIABLE) do
                        let values = Map.map(fun v e -> sol.Model.Eval(e,true)) vars in            
                        let res = Map.map(fun n e -> e.ToString()) values in            
                        let solution = Solution.Construct(res)                                             
                                                
                        //assert optimization constraint                    
                        let optim = Constraint.BTerm(Constraint.BComp(Constraint.Lt(e,e.subst solution.vars)))
                        sol.Assert(encoding.EncodeBExpr optim)                          

                        yield solution, optim
                    }                 

            //a sequence of equivalent (optimal) solutions
            let optimal_solutions =    
                let sol = z3.MkSolver()
                sol.Assert(cst)        
                
                //take the optimization function for the one before last (before the problem became unsat)
                let optim = 
                    optimization_solutions
                    |> Seq.toList
                    |> List.rev
                    |> List.tail
                    |> List.head
                    |> snd
                sol.Assert(encoding.EncodeBExpr optim)                          
                                   
                                   
                //extract the unique variables
                let unique_vars = 
                    vars
                    |> Map.filter(fun v _ -> model.system.varDefs.[v.Name].unique)                        
                    |> Map.toSeq           


                let cnt = ref 0
                seq {
                    while ((limit.IsNone || !cnt < limit.Value) && sol.Check()=Microsoft.Z3.Status.SATISFIABLE) do
                        let values = vars |> Map.map(fun _ e -> sol.Model.Eval(e,true))
                        let res = values |> Map.map(fun _ e -> e.ToString())
                        yield(Solution.Construct(res))   
            
                        //increment the number of solutions
                        cnt := !cnt + 1
                                                
                        //assert uniqueness constraint
                        unique_vars
                        |> Seq.map(fun (v, e) -> z3.MkNot(z3.MkEq(e,values.[v])))                       
                        |> Seq.toArray
                        |> fun e -> z3.MkOr(e)                        
                        |> fun e -> sol.Assert(e)
                    }

            //return the equivalent(optimal) solutions            
            optimal_solutions

        | Settings.BitVector(size) ->
            //let encoding = Encodings.Encoding.MkEncoding (Encodings.Encoding.MkBVEncoding size) model
            failwith "Not implemented yet"
      




//prototype functionality for extra output while generating solutions
let Z3SolveIterHK (z3:Microsoft.Z3.Context) updater (constraints, (variables:Map<Var.Var,Microsoft.Z3.Expr>)) = 
    let sol = z3.MkSolver() in
    sol.Assert([|constraints|]);          

    let cnt = ref 0.0 in
    let cancel = ref false in
    while (sol.Check()=Microsoft.Z3.Status.SATISFIABLE && not !cancel) do
        let values = Map.map(fun v e -> sol.Model.Eval(e,true)) variables in
        let res = Map.map(fun n e -> e.ToString()) values in                                                      
        let resstr = res |> Map.toSeq |> Seq.map (fun (vn, vv) -> vn.Name, vv) |> List.ofSeq in

        //apply the updater to send the results
        updater !cnt  resstr cancel;
                            
        //TODO: for now, all variables are treated as unique
        let cst = Seq.map(fun (v,e) -> z3.MkNot(z3.MkEq(variables.[v],values.[v]))) (variables |> Map.toSeq) |> Array.ofSeq in
        sol.Assert(z3.MkOr(cst))
        cnt := !cnt + 1.0

let BMCIterHK (model:Model)  updater =                 
    //construct a Z3 context (currently, with default options)
    let z3 = new Microsoft.Z3.Context() in   

    //perform basic sanity checkes (TODO: is this the right place for such functionality?)              
    model
    |> Checks.TypeCheck

    //solve the problem (Main BMC tactic)
    model
    |> Tactics.InlinePredicates  //TODO: PREDICATES ARE INLINED AS PART OF THE Z3 ENCODING....IS THIS NEEDED!?
    |> Tactics.DynamicsToConstraints
    |> Tactics.GenerateTypeBounds
    |> Encodings.ToZ3BoolInt z3              
    |> Z3SolveIterHK z3 updater










let BMCCore (model:Model) =                 
    //construct a Z3 context (currently, with default options)
    let z3 = new Microsoft.Z3.Context() in   

    //perform basic sanity checkes (TODO: is this the right place for such functionality?)              
    model
    |> Checks.TypeCheck

    //solve the problem (Main BMC tactic)
//    model
//    |> Lib.benchmark "Inlining" Tactics.InlinePredicates  //TODO: PREDICATES ARE INLINED AS PART OF THE Z3 ENCODING....IS THIS NEEDED!?
//    |> Lib.benchmark "DynamicsToConstraints" Tactics.DynamicsToConstraints
//    |> Lib.benchmark "Type Bounds" Tactics.GenerateTypeBounds
//    |> Lib.benchmark "Encoding" Encodings.ToZ3BoolIntUNSATCORE z3          
//    |> Lib.benchmark "Solving" Z3SolveCore z3
////
    model
    |> Tactics.InlinePredicates  //TODO: PREDICATES ARE INLINED AS PART OF THE Z3 ENCODING....IS THIS NEEDED!?
    |> Tactics.DynamicsToConstraints
    |> Tactics.GenerateTypeBounds
    |> Encodings.ToZ3BoolIntUNSATCORE z3          
    |> Z3SolveCore z3