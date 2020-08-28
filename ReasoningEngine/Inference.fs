module Microsoft.Research.ReasoningEngine.Inference
open Model
open Solution
open Solver


open Model




type InferSolution = 
    | ISBool of Microsoft.ML.Probabilistic.Distributions.Bernoulli
    | ISDisc of Microsoft.ML.Probabilistic.Distributions.Discrete

let InferSolve (variables:Map<Var.Var,Encodings.InferVarBase>) = 
    let ie = Microsoft.ML.Probabilistic.Models.InferenceEngine()
    //ie.Algorithm = Microsoft.ML.Probabilistic.InferenceEngine.GetBuiltInAlgorithms().[3]
    
    let n = variables.Count
       
    variables 
    |> Map.toSeq
    |> Seq.mapi(fun i (k,v) -> 
        System.Console.Error.WriteLine("Inferring {0}/{1}...", i, n)        
        let r = 
            match v with
            | Encodings.IBool(v,_) -> ISBool(ie.Infer<Microsoft.ML.Probabilistic.Distributions.Bernoulli>(v))
            | Encodings.IDisc(v,_) -> ISDisc(ie.Infer<Microsoft.ML.Probabilistic.Distributions.Discrete>(v))        
        (k.Name,r)
    )
    |> Map.ofSeq
                    

let BMCInference (model:Model) =                       
    model
    |> Checks.TypeCheck   //perform basic sanity checkes (TODO: is this the right place for such functionality?)        
    |> Tactics.InlineFixpoint //replace fixpoint constraints with expressions      
    |> Tactics.InlinePredicates  //TODO: PREDICATES ARE INLINED AS PART OF THE Z3 ENCODING....IS THIS NEEDED!?
    |> Tactics.DynamicsToConstraints //unroll trajectories and convert update rules to expressions    
    |> Encodings.InferEncoding.Encode    //encode
    |> InferSolve                      //solve (single solution)

