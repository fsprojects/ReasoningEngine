module Microsoft.Research.REIN.Procedures

module Expressions = Microsoft.Research.ReasoningEngine.Constraint
module Solver = Microsoft.Research.ReasoningEngine.Solver
module Var = Microsoft.Research.ReasoningEngine.Var




//convenience function, calling the solver to perform analysis
let Solve problem = 
    problem
    |> Translation.Translate 
    |> Solver.BMC 

let Enumerate problem = 
    problem
    |> Translation.Translate 
    |> Solver.BMCIter 

let EnumerateN n problem = 
    problem
    |> Translation.Translate 
    |> Solver.BMCIterN n
