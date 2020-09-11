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

  