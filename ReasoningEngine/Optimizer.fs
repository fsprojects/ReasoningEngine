module Microsoft.Research.ReasoningEngine.Optimizer
open Model
open Solution
open Solver



type Goal = Minimize | Maximize    

let Optimize (goal:Goal) (e:Constraint.NExpr) (limit: int option) (model:Model) =            
    
    let optimFn = 
        match goal with
        | Minimize -> Constraint.Gt
        | Maximize -> Constraint.Lt
    
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
                        let optim = Constraint.BTerm(Constraint.BComp(optimFn(e,e.subst solution.vars)))
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
            failwith "Optimization is not yet implemented for bit-vector REIL encodings"


//Generate all equivalent (minimal) solutions
let Minimize (e:Constraint.NExpr) (model:Model) =            
    Optimize Goal.Minimize e None model

//Generate all equivalent (maximal) solutions
let Maximize (e:Constraint.NExpr) (model:Model) =            
    Optimize Goal.Maximize e None model

//Generate N equivalent (minimal) solutions
let MinimizeN (N: int) (e:Constraint.NExpr) (model:Model) =            
    Optimize Goal.Minimize e (Some(N)) model

//Generate N equivalent (maximal) solutions
let MaximizeN (N: int) (e:Constraint.NExpr) (model:Model) =            
    Optimize Goal.Maximize e (Some(N)) model
