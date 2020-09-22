module Microsoft.Research.REIN.Translation


open Microsoft.Research.REIN.REIN
open Microsoft.Research.REIN.Regulation

open System.Collections.Generic
open Microsoft.Research.ReasoningEngine.Var
open Microsoft.Research.ReasoningEngine.Constraint
open Microsoft.Research.ReasoningEngine.Model
open Microsoft.Research.ReasoningEngine.Dynamics


let Translate (problem:Problem) = 
    let memoization = Memoization.Memoize problem


    //Define a boolean system variable for each possible interaction
    let cst = new List<Fact>();
    let iVars = 
        problem.interactions
        |> Seq.filter(fun i -> not i.definite)
        |> Seq.map(fun i -> i.var,VarDef.DeclareUniqueVar(System,Bool,i.var))

    //define an enumerator variable for each logic choice
    let max_reg_cond = Settings.maxConditions.[problem.settings.regulation] //max regulation condition
    let lVars = 
        problem.species
        |> Seq.map(fun s -> 
                    let vdef = 
                        match problem.settings.uniqueness with
                        | Settings.Interactions -> VarDef.DeclareVar(System,BNat(max_reg_cond),s.lVar)
                        | _                     -> VarDef.DeclareUniqueVar(System,BNat(max_reg_cond),s.lVar)

                    let v = SysVar(s.lVar) in
                    
                    //constraints on regulation conditions
                    match s.reg_conds with 
                    | None -> () //no constraints on regulation conditions
                    | Some(rc) -> 
                        let c = rc |> Seq.map(fun r -> BTerm(BComp(Eq(NTerm(NVar(v)),NTerm(NConst(r)))))) |> LOr
                        cst.Add({expr=c; info=Some("allowed logic choices for " + s.name)});                         

                    //activator required 
                    if s.NeedsActivator then cst.Add({expr = Not(memoization.not_inducible.[s.name]); info = Some("Activator required for " + s.name)})
 
                    s.lVar, vdef) 
                        
    //initialize FE and KO (perturbation) variables
    let pVars = Seq.fold(fun acc s -> 
        let a = if s.KO then Seq.singleton (s.KoVar,VarDef.DeclareVar(Path,Bool,s.KoVar)) else Seq.empty
        let b = if s.FE then Seq.singleton (s.FeVar,VarDef.DeclareVar(Path,Bool,s.FeVar)) else Seq.empty
        acc |> Seq.append a |> Seq.append b) Seq.empty problem.species


    //initialize the state variables
    let sVars = 
        problem.species 
        |> Seq.map(fun s -> 
            let v = 
                match problem.settings.uniqueness with
                | Settings.Uniqueness.All -> VarDef.DeclareUniqueVar(Scope.State,Bool,s.name)
                | _                       -> VarDef.DeclareVar(Scope.State,Bool,s.name)
            s.name, v)


    let regulation_conditions = Regulation.GenerateDefaultRegCond memoization problem.settings.regulation

    //Update functions (for a given species)
    let UpdateFn (s:Species) =
        let allowed_regulation_conditions = 
            match s.reg_conds with
            | None -> [0..max_reg_cond]
            | Some(rc) -> rc        

        let update rc = regulation_conditions.[rc] s.name //apply the regulation function with id=rc to species s
        let expr = match (s.KO,s.FE) with
                    | true,true -> LAnd(Seq.map(fun rc -> Imp(BTerm(BComp((Eq(NTerm(NVar(SysVar(s.lVar))),NTerm(NConst(rc)))))), 
                                                            Or(And(update rc, Not(BTerm(BVar(AbsPathVar(s.KoVar))))),
                                                                BTerm(BVar(AbsPathVar(s.FeVar)))))) allowed_regulation_conditions)                                  
                    | true, false -> LAnd(Seq.map(fun rc -> Imp(BTerm(BComp(Eq(NTerm(NVar(SysVar(s.lVar))),NTerm(NConst(rc))))), 
                                                             And(update rc, Not(BTerm(BVar(AbsPathVar(s.KoVar))))))) allowed_regulation_conditions)                                  
                    | false, true -> LAnd(Seq.map(fun rc -> Imp(BTerm(BComp(Eq(NTerm(NVar(SysVar(s.lVar))),NTerm(NConst(rc))))), 
                                                                Or(update rc,BTerm(BVar(AbsPathVar(s.FeVar)))))) allowed_regulation_conditions)                                  
                    | false,false -> LAnd(Seq.map(fun rc -> Imp(BTerm(BComp(Eq(NTerm(NVar(SysVar(s.lVar))),NTerm(NConst(rc))))), update rc)) allowed_regulation_conditions)        
        UpdateRule.Create(s.name,BExpr(expr))


    let system_update = 
        match problem.settings.updates with
        | Settings.Synchronous -> [Update.Create(None,Seq.map(fun s -> UpdateFn s) problem.species)]
        | Settings.Asynchronous -> problem.species |> Seq.map(fun s -> Update.Create(None, [UpdateFn s])) |> List.ofSeq
           

    //format the output as a ReasoningEngine problem
    let vars = 
        sVars                   //species variables
        |> Seq.append iVars     //interaction variables
        |> Seq.append lVars     //regulation condition variables
        |> Seq.append pVars     //perturbation variables
        |> Map.ofSeq
    
    //handle the interaction limit constraint   
    match problem.settings.interaction_limit with
    | None -> ()
    | Some(n) -> 
        if Seq.isEmpty iVars then () //no optional interactions
        else // We look for all solutions that have a maximum of the defined interaction limit
            let num_interactions_term = 
                iVars
                |> Seq.map fst
                |> Seq.map(fun v -> BTerm(BVar(SysVar(v))))
                |> Card
                |> NTerm                                    
            cst.Add(Fact.Create(BTerm(BComp(Le(num_interactions_term,NTerm(NConst(n))))),Some("Interaction limit")))
           

    //repeated interactions constraint
    if problem.settings.preventRepeatedInteractions then    
        let repeatedInteractionCst = 
            problem.interactions
            |> Seq.filter (fun i -> not i.definite) 
            |> Seq.map(fun i -> (i.source, i.target), i) 
            |> Seq.groupBy fst
            |> Seq.choose(fun (_,I) ->             
                let Ia = I |> Seq.toArray |> Array.map snd
                if Ia.Length = 2 then            
                    let v = Var.SysVar Ia.[0].var
                    let v' = Var.SysVar Ia.[1].var                
                    Some (Not(And(BTerm(BVar v), BTerm(BVar v'))))
                elif Ia.Length > 2 then            
                    failwith "Too many interactions"
                else
                    None            
                )
            |> LAnd
        cst.Add(REIN.Fact.Create(repeatedInteractionCst, Some "Repeated interactions constraint"))
        

    //return output
    let spec = 
        { predicates = problem.predicates
        ; facts = cst |> Seq.map(fun x -> x.expr)
        ; observations = problem.constraints |> Seq.map(fun x -> x.expr,if x.info.IsSome then x.info.Value else "")}  
    
    let settings = 
        { Microsoft.Research.ReasoningEngine.Settings.defaultSettings with 
            min_traj_length   = problem.settings.traj_length            
        }   

    { system      = DSystem.NewSystem(vars, system_update)
    ; constraints = spec
    ; settings    = settings
    }

    
let Parse str =    
    str 
    |> ParseAST     
    |> Translate
   