module Microsoft.Research.REIN.Export


open Microsoft.Research.REIN.REIN


let Decode (model:Microsoft.Research.REIN.REIN.Problem) (solution:Microsoft.Research.ReasoningEngine.Solution.Solution) = 
    //instantiate the regulation conditions of species    
    let species' = 
        model.species
        |> Seq.map(fun s -> {s with reg_conds = Some([System.Int32.Parse(solution.vars.[s.lVar])])})

    //instantiate the interactions
    let interactions' = 
        seq {
            for i in model.interactions do
                if i.definite then
                    yield i //always include definite interactions
                else
                    if solution.vars.[i.var] = "true" then
                        yield (i.MkDefinite()) //include optional interactions chosen in the solution as definite
            }
                
    {model with species = species'; interactions = interactions'; solution = Some solution}
    

//BY: This function could be generalized to convert REIL solutions to concrete REIN models
let SolutionToInteractions (sol:Microsoft.Research.ReasoningEngine.Solution.Solution) = 
    sol.vars
    |> Map.toSeq
    |> Seq.filter(fun (name,_) -> name.Contains("Pos_") || name.Contains("Neg_")) //select interaction variables only
    |> Seq.filter(fun (_,value) -> value = "true") //selected interactions only
    |> Seq.map(fun (name, value) ->                 
        let i = name.Split('_')
        let pos = i.[0] = "Pos"
        { source = i.[1]
        ; target = i.[2]
        ; positive = pos
        ; definite = false
        ; var = name}        
        )


//
//let SolutionToTrajectories (sol:Microsoft.Research.ReasoningEngine.Solution.Solution) = 
//    //find state variables
//    sol.t
//
//
//
//
//
//    sol.vars
//    |> Map.toSeq    
//    |> Seq.filter(fun (_,value) -> value = "true") //selected interactions only
//    |> Seq.map(fun (name, value) ->                 
//        let i = name.Split('_')
//        let pos = i.[0] = "Pos"
//        { source = i.[1]
//        ; target = i.[2]
//        ; positive = pos
//        ; definite = true
//        ; var = name}        
//        )

    
let TableToString table = 
    Seq.map(fun row ->
        Seq.map(fun v -> if v then "1" else "0") row        
        |> Seq.reduce(fun a b -> a + ", " + b)
        ) table
    |> Seq.reduce(fun a b -> a + "\n" + b)




//return a data structure that represents a summary of a number of solutions
let Summarize  (solutions:seq<Microsoft.Research.ReasoningEngine.Solution.Solution>) = 
    let summary = new System.Collections.Generic.Dictionary<Interaction,Set<int>>()
    
    //a function that adds a given solution id to the summary for a given interaction
    let addSolution id interaction = 
        if not (summary.ContainsKey(interaction)) then
            summary.Add(interaction, Set.empty) //create a slot for the interaction            
        
        //if interaction.definite then //the interaction was included
        let cs = summary.[interaction] //store the current list of solutions containing the interaction
        summary.Remove(interaction)|> ignore    //temporarily remove the interaction from the summmary
        summary.Add(interaction, Set.add id cs ) //add a new list where the current solution is appended                            

    //construct the summary
    if Seq.isEmpty solutions then //no solutions -> return an empty summary
        summary//, null
    else        
        //construct a sparse dictionary representation of the summary
        solutions
        |> Seq.iteri(fun id s -> 
            s
            |> SolutionToInteractions
            |> Seq.iter(fun i -> addSolution id i) //add solution interactions to the summary
            )

//        //construct an array representation of the summary
//        let M = Seq.length solutions
//        let N = Seq.length summary.Keys
//        let summaryArray = Array2D.init M N (fun i j -> false)
//        
//        Seq.iteri(fun i interaction ->             
//            Seq.iter(fun id ->  summaryArray.[id, i] <- true) summary.[interaction]
//            ) summary.Keys
                            
        summary//, summaryArray




type SimpleExpression = 
    |Fixpoint of string * int     //experiment * timestep
    |NotFixpoint of string * int  //experiment * timestep (negation)
    |KOVar of string * string * bool   //experimetn * species * value
    |FEVar of string * string * bool   //experimetn * species * value
    |State of string * int * string * bool  //experimetn * timestep * species * value
    override this.ToString() = 
        match this with
        |Fixpoint(p,k) -> p + " at step " + k.ToString() + " is a fixpoint."
        |NotFixpoint(p,k) -> p + " at step " + k.ToString() + " is NOT a fixpoint."
        |KOVar(p,s,c) -> (if c then "" else "No ") + "KO of " + s + " during experiment " + p
        |FEVar(p,s,c) -> (if c then "" else "No ") + "FE of " + s + " during experiment " + p
        |State(p,k,s,c) -> s + " is " + (if c then " ON " else " OFF ") + "at step " + k.ToString() + " during experiment " + p

let SummarizeSpec(E:seq<SimpleExpression>) = 
    //find out all the paths and steps    
    let paths,species, KO, FE = 
        Seq.fold(fun (pacc:Set<string*int>,sacc:Set<string>, kacc:Set<string*string>, facc:Set<string*string>) e -> 
            match e with 
            |Fixpoint(p,k) -> pacc.Add (p,k), sacc, kacc, facc
            |NotFixpoint(p,k) ->  pacc.Add (p,k), sacc, kacc, facc
            |State(p,k,s,c) -> pacc.Add (p,k), sacc.Add s, kacc, facc
            |KOVar(p,s,c) -> pacc.Add(p,0), sacc.Add s, kacc.Add (p,s), facc
            |FEVar(p,s,c) -> pacc.Add(p,0), sacc.Add s, kacc, facc.Add (p,s)
        ) (Set.empty,Set.empty, Set.empty, Set.empty) E           
    let variables = 
        let KOvars = KO |> Set.map(fun f -> "KO(" + snd f + ")")
        let FEvars = FE |> Set.map(fun f -> "FE(" + snd f + ")")
        
        species
        |> Set.union KOvars
        |> Set.union FEvars
//        Seq.append (species |> Set.toSeq) 
//            (Seq.append (KO |> Set.toSeq |> Seq.map(fun 
//                            (FE |> Set.toSeq |> Seq.map(fun f -> "FE(" + snd f  + ")")))
        |> Array.ofSeq
    let p2str p k = p + "[" + k.ToString() + "]"
    let experiments = 
        paths 
        |> Seq.map(fun (p,k) -> p2str p k)
        |> Array.ofSeq

    let values = Array2D.init variables.Length experiments.Length (fun i j -> 0)
    let fpvalues = Array.init experiments.Length (fun i -> 0)

    let expId p k = Array.findIndex(fun x -> x = (p2str p k)) experiments
    let varId s = Array.findIndex(fun x -> x = s) variables

    Seq.iter(fun e -> 
         match e with 
            |Fixpoint(p,k) -> fpvalues.[expId p k] <- 1
            |NotFixpoint(p,k) ->  fpvalues.[expId p k] <- -1
            |State(p,k,s,c) -> values.[varId s, expId p k] <- if c then 1 else -1
            |KOVar(p,s,c) -> values.[varId ("KO(" + s + ")"), expId p 0] <- if c then 1 else -1
            |FEVar(p,s,c) -> values.[varId ("FE(" + s + ")"), expId p 0] <- if c then 1 else -1
    ) E
                        
    experiments, variables, values, fpvalues    



module Expressions = Microsoft.Research.ReasoningEngine.Constraint
module Vars = Microsoft.Research.ReasoningEngine.Var

let ExportSimpleExpression(E:seq<Expressions.BExpr>) =     
    let rec flatten (e:Expressions.BExpr) = 
        match e with
        |Expressions.And(a,b) -> List.append (flatten a) (flatten b)
        |Expressions.Not(Expressions.Beq(a,b)) -> 
            match a,b with
            |Expressions.BTerm(Expressions.BVar(v)),Expressions.BTerm(Expressions.BConst(c))
            |Expressions.BTerm(Expressions.BConst(c)), Expressions.BTerm(Expressions.BVar(v)) ->             
                match v with
                |Vars.StateVar(p,k,s) -> [State(p,k,s,not c)]
                |Vars.PathVar(p,s) -> 
                    if s.Contains("KO") then [KOVar(p,s.Replace("KO_",""),not c)]
                    else if s.Contains("FE") then [FEVar(p,s.Replace("FE_",""),not c)]
                    else failwith "Uknown path variable type"
                | _ -> failwith "Cannot apply function"
            | _ -> failwith "Cannot apply function"        
        |Expressions.BTerm(Expressions.BVar(Vars.StateVar(p,k,s))) -> [State(p,k,s,true)]
        |Expressions.Beq(a,b) -> 
            match a,b with
            |Expressions.BTerm(Expressions.BVar(v)),Expressions.BTerm(Expressions.BConst(c))
            |Expressions.BTerm(Expressions.BConst(c)), Expressions.BTerm(Expressions.BVar(v)) ->             
                match v with
                |Vars.StateVar(p,k,s) -> [State(p,k,s,c)]
                |Vars.PathVar(p,s) -> 
                    if s.Contains("KO") then [KOVar(p,s.Replace("KO_",""), c)]
                    else if s.Contains("FE") then [FEVar(p,s.Replace("FE_",""), c)]
                    else failwith "Unknown path variable type"
                | _ -> failwith "Cannot apply function"
            | _ -> failwith "Cannot apply function"
        |Expressions.Not(Expressions.BTerm(Expressions.Fixpoint(p,k))) -> [NotFixpoint(p,k)]
        |Expressions.BTerm(Expressions.Fixpoint(p,k)) -> [Fixpoint(p,k)]
        |Expressions.LAnd(terms) ->  terms |> Seq.collect(fun e -> flatten e) |> List.ofSeq        
        | _ -> failwith "Cannot apply function"

    //flatten and append all constraints        
    Seq.fold(fun acc e -> Seq.append acc (flatten e)) Seq.empty E

