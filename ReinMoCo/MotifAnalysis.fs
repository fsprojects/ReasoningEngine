module Microsoft.Research.REINMoCo.Analysis
open Microsoft.Research.ReasoningEngine.Constraint
open Microsoft.Research.REINMoCo

type ReinMoCoResultFlag = 
    | TriviallyRequired        //an instance of the motif with definite interactions was found
    | TriviallyDisallowed      //no valid instance of the motif exist
    | StructurallyRequired     //Motif is required without the constraints
    | StructurallyDisallowed   //Motif is dissalowed without the constraints
    | Required              
    | Disallowed    
    | Inconclusive          //solutions exist both with the motif and without it
    | Inconsistent          //solutions do exist neither with the motif nor without it
    static member fromString (s:string) =
        match s with 
            | "Required (trivially)"        -> TriviallyRequired         
            | "Disallowed (trivially)"      -> TriviallyDisallowed       
            | "Required (structurally)"     -> StructurallyRequired      
            | "Disallowed (structurally)"   -> StructurallyDisallowed    
            | "Required"                    -> Required                  
            | "Disallowed"                  -> Disallowed                
            | "Inconclusive"                -> Inconclusive              
            | "Inconsistent"                -> Inconsistent    
            | _ -> failwithf "Unknown motif analysis flag %s" s
    
    static member toString (f:ReinMoCoResultFlag) =
        match f with
            | TriviallyRequired    -> "Required (trivially)"
            | TriviallyDisallowed  -> "Disallowed (trivially)"
            | StructurallyRequired    ->"Required (structurally)"
            | StructurallyDisallowed  -> "Disallowed (structurally)"
            | Required             -> "Required"
            | Disallowed           -> "Disallowed"
            | Inconclusive         -> "Inconclusive"
            | Inconsistent         -> "Inconsistent"
        

type ReinMoCoResult = 
    { encodingTime: float
      hypothesisSolvingTime : float      
      nullSolvingTime: float      
      flag: ReinMoCoResultFlag
    }

    static member ToHtmlTable (results:seq<string*ReinMoCoResult>) =     
        results
        |> Seq.map(fun (name,result) ->
            let times = sprintf "<td>%f</td><td>%f</td><td>%f</td>" result.encodingTime result.hypothesisSolvingTime result.nullSolvingTime
            sprintf "<tr><td>%s</td><td>%s</td>%s</tr>" name (ReinMoCoResultFlag.toString result.flag) times
            )
        |> String.concat "\n"
        |> sprintf "<tr><th>Motif</th><th>Result</th><th>Encoding Time</th><th>Solving Time<br>(motif present)</th><th>Solving Time<br>(motif absent)</th></tr>%s"          


    static member ToTsvRow (motif:string, result:ReinMoCoResult) = 
        let num2str x = if x<0.0 then "" else sprintf "%f" x
        let times = sprintf "%s\t%s\t%s" (num2str result.encodingTime) (num2str result.hypothesisSolvingTime) (num2str result.nullSolvingTime)
        sprintf "%s\t%s\t%s" motif (ReinMoCoResultFlag.toString result.flag) times


    static member ToTsvRowApprox (motif:string, result:ReinMoCoResult) = 
        let num2str x = if x<0.0 then "" else sprintf "%.1f" x
        let times = sprintf "%s\t%s\t%s" (num2str result.encodingTime) (num2str result.hypothesisSolvingTime) (num2str result.nullSolvingTime)
        sprintf "%s\t%s\t%s" motif (ReinMoCoResultFlag.toString result.flag) times



    static member FromTsvRow (row:string) = 
        let fields = row.Split([|'\t'|], System.StringSplitOptions.RemoveEmptyEntries)
        let motif = fields.[0]
        let result = 
            { encodingTime = float fields.[2]
              hypothesisSolvingTime = if fields.Length > 3 then float fields.[3] else -1.0
              nullSolvingTime = if fields.Length > 4 then float fields.[4] else -1.0
              flag = ReinMoCoResultFlag.fromString fields.[1]
            }
        motif, result




    static member ToTsvTable (results:seq<string*ReinMoCoResult>) =     
        results
        |> Seq.map ReinMoCoResult.ToTsvRow            
        |> String.concat "\n"
        |> sprintf "Motif\tResult\tEncoding Time\tSolving Time (motif present)\tSolving Time (motif absent)\n%s"    
        
    
let MkConstrained (p:Problem) (motifs:seq<string*bool>) =     
    let MotifConstraint name value (m:Problem) =    
        let baseCst = BTerm(Predicate(AbsPred name))
        let cst = if value then baseCst else Not(baseCst)
        let fact = Microsoft.Research.REIN.REIN.Fact.Create(cst, Some (sprintf "Motif %s is %s" name (if value then "present" else "absent")))
        {m with constraints = Seq.append m.constraints (fact |> Seq.singleton)}
        |> Problem.InitMotifPredicates
                       
    motifs
    |> Seq.fold(fun acc (m,v) -> acc |> MotifConstraint m v) p //initial model includes motifs
    |> Problem.ToReinWithConstraints                           //TODO: constraints as input if pre-computed?


(*Given an motif specification (motifs with present or not flags) check if models exist *)
let CheckMotifCombination (p:Problem) (spec:seq<string*bool>) = 
    let t0 = System.DateTime.Now     
    let model = MkConstrained p spec        
    let result = model |>  Microsoft.Research.REIN.Translation.Translate |> Microsoft.Research.ReasoningEngine.Solver.BMC                           
    result.IsSome, (System.DateTime.Now - t0).TotalSeconds        


(* Determine the result (required/disallowed/etc) for a given motif spec *)
let CheckMotifCombinationWithTrivial (p:Problem) (spec:seq<string*bool>) = 
    let spec' = spec |> Seq.map(fun (m,v) -> m, not v)  //null hypothesis

    System.Console.Error.Write("# Generating motif constraints...")
    let t0 = System.DateTime.Now
    let motifConstraintRaw = spec  |> Seq.map (fun (m,v) -> m, Problem.MotifToConstraints p m, v) |> Seq.toArray            
    System.Console.Error.WriteLine("done ({0} sec)", (System.DateTime.Now - t0).TotalSeconds)
    System.Console.Error.Write("# Simplifying...")
    let motifConstraintSimplified = motifConstraintRaw |> Array.map (fun (m,expr,v) -> m, expr.Simplify(),v)
    let motifConstraint = 
        (motifConstraintSimplified
        |> Seq.map (fun (_, expr,v) -> if v then expr else Not expr)                                    
        |> BExpr.LAnd).Simplify()
    let encodingTime = (System.DateTime.Now - t0).TotalSeconds
    System.Console.Error.WriteLine("done (total {0} sec)", encodingTime)
            
    match motifConstraint with 
    | BTerm(BConst(true)) -> 
        { encodingTime = encodingTime         
          hypothesisSolvingTime = -1.0          
          nullSolvingTime = -1.0
          flag = TriviallyRequired
        }
    | BTerm(BConst(false)) -> 
        { encodingTime = encodingTime         
          hypothesisSolvingTime = -1.0          
          nullSolvingTime = -1.0
          flag = TriviallyDisallowed
        }

    | _ ->
        //The result is not trivial but might be obvious from the network's structure alone
        let p' = 
            {p with 
                predicates = motifConstraintSimplified |> Seq.map(fun (m,expr,_) -> m, expr) |> Map.ofSeq
                constraints = Seq.empty
                motifs = spec |> Seq.map(fun (m,_) -> m, p.motifs.[m])  |> Map.ofSeq
            } //no constraints model                        
        
        System.Console.Error.Write("# Checking hypothesis without constraints...")
        let motif_present, t1 = CheckMotifCombination p' spec  
        System.Console.Error.WriteLine("done ({0} sec)", t1)
        System.Console.Error.Write("# Checking hypothesis without constraints...")
        let motif_absent, t2  = CheckMotifCombination p' spec'
        System.Console.Error.WriteLine("done ({0} sec)", t2)

        let resultTemplate = 
            { encodingTime = encodingTime          
              hypothesisSolvingTime = t1
              nullSolvingTime = t2
              flag = Inconclusive
            }
        match motif_present, motif_absent with 
        | false, true -> {resultTemplate with flag = StructurallyDisallowed}
        | true, false -> {resultTemplate with flag = StructurallyRequired}
        | false, false -> {resultTemplate with flag = Inconsistent}
        | true, true -> 
            //Test the model with constraints                   

            System.Console.Error.Write("# Checking presence...")
            let motif_present, t1 = CheckMotifCombination p spec  
            System.Console.Error.WriteLine("done ({0} sec)", t1)
            System.Console.Error.Write("# Checking absence...")
            let motif_absent, t2  = CheckMotifCombination p spec'
            System.Console.Error.WriteLine("done ({0} sec)", t2)
            
            
            let resultTemplate = 
                { encodingTime = encodingTime          
                  hypothesisSolvingTime = t1
                  nullSolvingTime = t2
                  flag = Inconclusive
                }
            match motif_present, motif_absent with 
            | false, true -> {resultTemplate with flag = Disallowed}
            | true, false -> {resultTemplate with flag = Required}
            | false, false -> {resultTemplate with flag = Inconsistent}
            | true, true -> {resultTemplate with flag = Inconclusive}


(* For each motif in the model, determin if required/disallowed/etc*)
let AnalyzeMotifs (p:Problem) = 
    p.motifs
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.map(fun motifName -> motifName, CheckMotifCombinationWithTrivial p [motifName, true])


(*  Check the following properties for each unique pair of motifs (M1, M2):
        hypothesis: M1 && M2 
        null:       (not M1) && (not M2)
*)
let AnalyzeMotifPairs (p:Problem) =         
    let M = p.motifs|> Map.toArray |> Array.map snd

    seq {
        for i in [0..M.Length-2] do 
            for j in [i+1..M.Length-1] do
                let M1 = M.[i].name
                let M2 = M.[j].name
                yield M1, M2, CheckMotifCombinationWithTrivial p [M1, true; M2, true]
        }

            

let CheckMotif (p:Microsoft.Research.REIN.REIN.Problem) (motif:Motif)= 
    {Problem.FromREIN p with motifs = [motif.name, motif] |> Map.ofSeq} 
    |> AnalyzeMotifs
    |> Seq.head