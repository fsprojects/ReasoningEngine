module Microsoft.Research.ReasoningEngine.Equivalence
open Model
open Solution
open Constraint


let FindEquivalent (M1:Model) (M2:Model)=                 
//    //construct a Z3 context (currently, with default options)
//    let z3 = new Microsoft.Z3.Context() in   
//    
//    let state_vars1 = M1.system.varDefs |> Map.toSeq |> Seq.filter(fun (s,(v:Var.VarDef)) -> v.scope= Var.State) in
//    let state_vars2 = M2.system.varDefs |> Map.toSeq |> Seq.filter(fun (s,(v:Var.VarDef)) -> v.scope= Var.State) in
//
//    let s1 = state_vars1 |> Seq.head |> fst in
//    let s2 = state_vars2 |> Seq.head |> fst in
//
//    let M1 = {M1 with constraints=M1.constraints.AddFact(Beq(BTerm(BVar(Var.StateVar("default",1,s1))),BTerm(BVar(Var.StateVar("default",1,s1)))))} in
//    let M2 = {M2 with constraints=M2.constraints.AddFact(Beq(BTerm(BVar(Var.StateVar("default",1,s2))),BTerm(BVar(Var.StateVar("default",1,s2)))))} in
//
//    let cst1, v1 = 
//        M1
//        |> Tactics.InlinePredicates
//        |> Tactics.DynamicsToConstraints
//        |> Tactics.GenerateTypeBounds
//        |> Encodings.ToZ3BoolInt z3
//    in
//
//    let cst2, v2 = 
//        M2
//        |> Tactics.InlinePredicates
//        |> Tactics.DynamicsToConstraints
//        |> Tactics.GenerateTypeBounds
//        |> Encodings.ToZ3BoolInt z3
//    in
//
//    let sol = z3.MkSolver() in
//    
//   
//   
//    let state_vars1 = state_vars1 |> Map.ofSeq in
//    let state_vars2 = state_vars2 |> Map.ofSeq in
//   
//    let step1 = v1 |> Map.toSeq |> Seq.filter(fun (v,e) -> match v.Time with | Some(1) -> true | _ -> false) in
//    let step2 = v1 |> Map.toSeq |> Seq.filter(fun (v,e) -> match v.Time with | Some(2) -> true | _ -> false) in
//    
//
//    let vars = Seq.append(Seq.append(step1) step2) (v2 |> Map.toSeq |> Seq.filter(fun (v,e) -> match v.Time with | Some(_) -> true | _ -> false)) |> Seq.map snd in
//
//    let step1_eq = z3.MkAnd(Seq.map(fun (v,e) -> z3.MkEq(e,v2.[v]))  step1 |> Seq.toArray) in
//    let step2_eq = z3.MkAnd(Seq.map(fun (v,e) -> z3.MkEq(e,v2.[v]))  step2 |> Seq.toArray) in
//    
//    let vars = Seq.append(v1 |> Map.toSeq |> Seq.map snd) (v2 |> Map.toSeq |> Seq.map snd) |> Seq.toArray in
//    let cst = z3.MkAnd(cst1,cst2) in
//
//    let imp = z3.MkImplies(z3.MkAnd(step1_eq,cst),step2_eq) in    
//    let forall = z3.MkForall(vars,imp)  in
//    sol.Assert(forall); 
//
//    let variables = Seq.append (Map.toSeq v1) (Map.toSeq v2) in    
//    if (sol.Check()=Microsoft.Z3.Status.SATISFIABLE) then
//        let res = Seq.map(fun ((v:Var.Var),e) -> v,sol.Model.Eval(e,true).ToString()) variables in
//        Some(res)        
//    else
//        None    
    None


let FindFPEquivalent (M1:Model) (M2:Model)=                 
    ""    