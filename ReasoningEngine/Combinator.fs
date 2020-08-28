module  Microsoft.Research.ReasoningEngine.Combinator
open Model

//given models M1 and M2 produce a model M such that M = M1 || M2 (parallel composition)
let CombineModels (M1:DSystem) (M2:DSystem) = 
    if not (Checks.SharedVarCheck M1 M2) then failwith "Model variables scope or types mismatch";
    
    let varDefs = Seq.append(M1.varDefs |> Map.toSeq) (M2.varDefs |> Map.toSeq) 
                  |> Seq.distinctBy(fun x -> fst x)
                  |> Map.ofSeq in
    let updates = Seq.append(M1.updates) M2.updates
    DSystem.NewSystem(varDefs,updates)

//Given a list of models, return the comination of all of these
let CombineMultipleModels (M:seq<DSystem>) = Seq.reduce(fun a b -> CombineModels a b) M

//COMBINATOR MUST BE ABLE TO HANDLE PROBLEMS, NOT JUST MODELS
let Combine = CombineMultipleModels
let ExtractModuleType t parse str = 
        let pattern = System.Text.RegularExpressions.Regex("<" + t + ">[\w \n \r]*{([^}]*)};") in
        let matches = pattern.Matches(str) 
                      |> Seq.cast 
                      |> Seq.map(fun (m:System.Text.RegularExpressions.Match) -> m.Groups.Item(1).Value) in                
        (Seq.map(fun p -> parse p) matches, pattern.Replace(str,"")) 
    in        
let ExtractModules (parsers:Map<string,string->DSystem>) str =     
  
    let (modules,core) = Seq.fold(fun acc (t,fn) -> 
                                    let p = snd acc in //old program
                                    let m = fst acc in //old modules list
                                    let (nm,np) = ExtractModuleType t fn p in //new program and modules
                                    (Seq.append(m) nm, np)) (Seq.empty,str) (Map.toSeq parsers) in
//    let core = snd modules in
//    let modules = Seq.map(fun x -> x.model) (fst modules) in
    let n = Seq.length modules in
    (modules,core)


let CombineConstraints (C1:Constraints)  (C2:Constraints) = 
    let  predicates  = 
        Map.toSeq C1.predicates
        |> Seq.append (Map.toSeq C2.predicates)
        |> Seq.distinct
        |> Map.ofSeq
    let facts = 
        Seq.append C1.facts C2.facts
        |> Seq.distinct                    
    let observations = 
        Seq.append C1.observations C2.observations
        |> Seq.distinct
        
    { predicates = predicates
    ; facts = facts
    ; observations = observations }        

let MergeModels (M1:Model) (M2:Model) =
      let system = CombineModels M1.system M2.system
      let constraints =CombineConstraints  M1.constraints M2.constraints
      let options = M1.settings
      { system = system
      ; constraints = constraints
      ; settings = options }
             

let Merge (S1:DSystem) (S2:DSystem) =
      
      let S1_substs = Map.map(fun n _ -> "S1_" + n) S1.varDefs in
      let S2_substs = Map.map(fun n _ -> "S2_" + n) S1.varDefs in
      

      


      ""

//    
//    
//    let varDefs = Seq.append(M1.varDefs |> Map.toSeq) (M2.varDefs |> Map.toSeq) 
//                  |> Seq.distinctBy(fun x -> fst x)
//                  |> Map.ofSeq in
//    let updates = Seq.append(M1.updates) M2.updates
//    DSystem.NewSystem(varDefs,updates)


let Parse (parsers:Map<string,string->DSystem>) str =
    let (modules,core) = ExtractModules parsers str in    
    let M = Parse core in
    Combine (Seq.append(modules) [M.system])