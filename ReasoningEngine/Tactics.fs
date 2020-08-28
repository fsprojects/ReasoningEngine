module  Microsoft.Research.ReasoningEngine.Tactics
open Model
open System.Collections.Generic
open Constraint


let InlinePredicates (model:Model) = 
    //let predicates = Map.map(fun n (p:BExpr) -> p.Expr) model.constraints.predicates in     //extract the set of predicates
    let predicates = model.constraints.predicates in     //extract the set of predicates
    let new_cst = Seq.map(fun (cst:BExpr) -> cst.Inline predicates) model.constraints.facts in //inline the predicates in each constraint
    let new_obs = Seq.map(fun (e:BExpr,d) -> e.Inline predicates,d) model.constraints.observations in
    let new_spec = {predicates = Map.empty; facts = new_cst; observations = new_obs} in //construct the new set of inlined specifications
    {system = model.system; constraints = new_spec; settings = model.settings}    //construct and return the new problem




let InlineFixpoint (model:Model) =   
    if TProbes.IsNonMarkovian model.system then failwith "The current fixpoint constraint implementation handles Markovian systems only (no delays)"

    //given an update rule x' = f(a,b,c....), generate the constraint p[k].x = f(p[k+1].a,p[k+1].b,...)
    //if a guard is specified, then the rule becomes g => update_constraints (i.e. consider only enabled constraints)
    let fixpoint p k =  
        model.system.updates
        |> Seq.map(fun update ->             
            let update_constraints = 
                update.rules
                |> Seq.map(fun u -> 
                    match u with
                    | Dynamics.Assignment(a) -> 
                        let var = Var.StateVar(p,k,a.var)
                        let var' = a.expr.Init p (k+1)
                    
                        match model.system.varDefs.[a.var].t with
                        | Var.Bool ->
                            let be = 
                                match var' with
                                | BExpr(b) -> b
                                | _ -> failwith "incompatible variable types in fixpoint constraints (dynamics)"
                            Beq(BTerm(BVar(var)),be)
                        | _ -> 
                            let ne = 
                                match var' with
                                | NExpr(n) -> n
                                | _ -> failwith "incompatible variable types in fixpoint constraints (dynamics)"
                            BTerm(BComp(Eq(NTerm(NVar(var)),ne)))
                    | Dynamics.Relation(_) -> failwith "Fixpoint constraints are not yet implemented for relation update rules"            
                    )
                |> LAnd
            match update.guard with
            | Some(g) -> Imp(g.Init p k, update_constraints)
            | None ->  update_constraints
            )
        |> LAnd

    let apply_fixpoint (t:BTerm) = 
        match t with
        | Fixpoint(p,k) -> fixpoint p k //apply the function for fixpoints, leave all other terms unchanged
        | BComp(c) -> BTerm(BComp(c))
        | BConst(b) -> BTerm(BConst(b))
        | BVar(v) -> BTerm(BVar(v))
        | Predicate(p) -> BTerm(Predicate(p))
        | Terminal(p,k) -> BTerm(Terminal(p,k))

    let facts' = model.constraints.facts |> Seq.map(fun cst -> cst.map apply_fixpoint)                
    let observations' = model.constraints.observations |> Seq.map(fun (e,d) -> e.map apply_fixpoint,d)  in
    let constraints' = 
        { predicates = model.constraints.predicates
        ; facts = facts'
        ; observations = observations'}

    //return the results
    { system = model.system
    ; constraints = constraints'
    ; settings = model.settings}





let DynamicsToConstraints (model:Model) =     
    let paths = 
        Constraint.GetAllPaths (model.constraints.GetConstraints()) //extract all paths from the specification
        |> Seq.map(fun (a,b,c) -> if c < model.settings.min_traj_length then (a,b,model.settings.min_traj_length) else (a,b,c))//replace the path length with the absolute min traj length


    //extract all state variables with their types
    let stateVars = Seq.map(fun (v,def:Var.VarDef) -> v, def.t) 
                        (model.system.varDefs 
                        |> Map.toSeq 
                        |> Seq.filter(fun (_,def) -> def.scope=Var.State))
                    |> Map.ofSeq in

    //generate constraints to represent the dynamics of the system
    let update_cst = Dynamics.GenerateConstraints paths stateVars model.system.updates in        
        
    //compile all constraints    
    let spec_with_updates = Seq.append(model.constraints.facts) update_cst in

    //construct the modified spec with update constraints
    let new_spec = {model.constraints with facts=spec_with_updates}

    //return the modified problem with updates encoded as constraints
    {model with constraints=new_spec; system=DSystem.NewSystem(model.system.varDefs, Seq.empty)}


//NOTE: potential code duplication for instantiating constraints with Dynamics.GenerateConstraints
let GenerateTypeBounds (model: Model) = 

    //extract all paths from the specification
    let paths = Constraint.GetAllPaths (model.constraints.GetConstraints()) in

    let GE n v = BTerm(BComp(Ge(NTerm(NVar(v)),NTerm(NConst n)))) in
    let LE n v = BTerm(BComp(Le(NTerm(NVar(v)),NTerm(NConst n)))) in
    let In n1 n2 v = And(BTerm(BComp(Ge(NTerm(NVar(v)),NTerm(NConst n1)))),BTerm(BComp(Le(NTerm(NVar(v)),NTerm(NConst n2))))) in

    //apply the constraint to all paths and states
    //NOTE: these procedures can be generalized as quantifiers in the language?
    //function v produces a constraint given a variable    
    let forall_states f v=     
            Seq.map(fun (p,min_k,max_k) -> 
                Seq.map(fun k -> 
                    let var = Var.StateVar(p,k,v) in //instantiate the var with path and timestep
                    f(var)) [min_k..max_k] ) paths
            |> Seq.fold(fun acc s -> Seq.append(acc) s) Seq.empty in
    let forall_paths f v =             
            Seq.map(fun (p,_,_) ->                 
                    let var = Var.PathVar(p,v) in //instantiate the var with path and timestep
                    f(var)) paths            

    //generate constraints for all varialbes and all paths
    let bound_constraints, newVarDefs = 
        Seq.fold(fun (a:seq<Constraint.BExpr>,b) (v:string*Var.VarDef) -> 
            let var_name = fst v in
            let var = snd v in
                        
            let quantifier f v =                 
                match var.scope with
                | Var.Scope.State -> forall_states f v
                | Var.Scope.Path -> forall_paths f v
                | Var.Scope.System -> let var = Var.SysVar(v) in Seq.singleton(f(var))
            in            

            //generate constraints and substitute variable as integers
            match var.t with
            | Var.Nat -> 
                let cst_gen = GE 0 in           
                let cst = quantifier cst_gen var_name in
                let var' = 
                    if var.unique then 
                        Var.VarDef.DeclareUniqueVar(var.scope,Var.Int,var.name)
                    else
                        Var.VarDef.DeclareVar(var.scope,Var.Int,var.name)
                let new_var = (var_name, var')
                (Seq.append(a) cst, Seq.append(b) [new_var])
            | Var.BNat n -> 
                let cst_gen = In 0 n in           
                let cst = quantifier cst_gen var_name in
                let var' = 
                    if var.unique then 
                        Var.VarDef.DeclareUniqueVar(var.scope,Var.Int,var.name)
                    else
                        Var.VarDef.DeclareVar(var.scope,Var.Int,var.name)
                let new_var = (var_name, var')
                (Seq.append(a) cst, Seq.append(b) [new_var])
            | Var.BInt n -> 
                let cst_gen = In -n n in            //NOTE: currently, the bound is enforced as an upper and lower bound
                let cst = quantifier cst_gen var_name in
                let var' = 
                    if var.unique then 
                        Var.VarDef.DeclareUniqueVar(var.scope,Var.Int,var.name)
                    else
                        Var.VarDef.DeclareVar(var.scope,Var.Int,var.name)
                let new_var = (var_name, var')
                (Seq.append(a) cst, Seq.append(b) [new_var])
            | _ -> (a, Seq.append(b) [v])                                   
            ) (Seq.empty,Seq.empty) (model.system.varDefs |> Map.toSeq) in
    
    let newVarDefs = Map.ofSeq(newVarDefs) in
    
    //construct the modified system with changed variable types
    let new_system = DSystem.NewSystem(newVarDefs, model.system.updates) in

    //compile all constraints    
    let spec_with_bounds = Seq.append(model.constraints.facts) bound_constraints in

    //construct the modified spec with update constraints
    let new_spec = {model.constraints with facts=spec_with_bounds}

    //return the modified problem with updates encoded as constraints
    {model with constraints=new_spec; system=new_system}




//
//
//let ToInt (model:Model) = 
//    let predicates = model.constraints.predicates in
//    let spec = model.constraints.facts in
//    let varDefs = Map.map(fun n v -> {v with Var.t=Var.Int}) model.system.varDefs in
//
//    //TODO: GENERATE CONSTRAINTS FOR ALL SUBTYPES!
//            
//    let rec EncodeBExpr (e:BExpr)  =
//        match e with
//        | BTerm(e1) -> EncodeCExpr e1                
//        | BConst b -> if b then z3.MkTrue() else z3.MkFalse()
//        | Beq(e1,e2) -> z3.MkEq(EncodeBExpr e1, EncodeBExpr e2)
//        | Imp(e1,e2) -> z3.MkImplies(EncodeBExpr e1, EncodeBExpr e2)
//        | And(e1,e2) -> z3.MkAnd(EncodeBExpr e1, EncodeBExpr e2)
//        | Or(e1,e2) -> z3.MkOr(EncodeBExpr e1, EncodeBExpr e2)
//        | LAnd(el) -> z3.MkAnd(Seq.map(fun (e:BExpr) -> EncodeBExpr e) el |> Seq.toArray)
//        | LOr(el) -> z3.MkOr(Seq.map(fun (e:BExpr) -> EncodeBExpr e) el |> Seq.toArray)
//        | Not(e1) -> z3.MkNot(EncodeBExpr e1)            
//    in              
//
//
//    let cst = z3.MkAnd(Seq.map(fun x -> EncodeBExpr x) spec |> Array.ofSeq) in
//    (cst,vars)