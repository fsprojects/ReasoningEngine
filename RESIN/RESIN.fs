(* 
----------------------------
            NOTES

1. Currently, the NeedsActivator option is hard-coded to false for all species
2. Update the regulation conditions according to REIN
----------------------------
*)

module Microsoft.Research.RESIN.RESIN
open Microsoft.Research.ReasoningEngine.Var
open Microsoft.Research.ReasoningEngine.Constraint
open Microsoft.Research.ReasoningEngine.Model
open Microsoft.Research.ReasoningEngine.Dynamics
open System.Collections.Generic
open Parser
open Microsoft.Research.REIN.REIN

type Cell = {
     cname: string;
     interactions: seq<Interaction>;
    }

type Switch = {
    cell1: string;
    cell2: string;
    condition: BExpr;
    }

type Problem = {
    labels: seq<string * BExpr>;
    sync: bool; // synchronoucs update ?
    species:seq<Species>;
    cells: seq<Cell>;
    switches: seq<Switch>;
    constraints: seq<Fact>;
    fixedLabel: string;
    }


let ParseText str = 
        let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString(str)   in                                                     
        try
            Parser.start Lexer.tokenize lexbuf 
        with e ->                                
            let pos = lexbuf.EndPos in
            let line = pos.Line in
            let column = pos.Column in
            let linestr = str.Split('\n').[line] in
            let len = 10 in
            let from = if column>len then column-len else 0 in
            let until = if column<(linestr.Length-len) then column+len else linestr.Length-1 in           
            raise (new System.Exception(System.String.Format("RE:SIN({0},{1}):error : parsing failed at line {0}, column {1}: {2}",line+1,column,e.Message)));    


let ToInteraction cell_name (a,b,c,d) =    
    let var_name = cell_name + "_" + (if c then "Pos_" else "Neg_") + a + "_" + b 
    {source=a; target=b; positive=c; definite=d; var=var_name}

let SplitFields spec = 
    Seq.fold(fun (predicates, sync, species, cells, switches, constraints) x -> 
                                                           match x with 
                                                           | Assign(n,e) -> ((n,e)::predicates, sync, species, cells, switches, constraints) 
                                                           | Sync(b) -> (predicates, (b)::sync, species, cells, switches, constraints)
                                                           | Cell(cell_name,b) -> 
                                                                let interactions = b |> Seq.map (ToInteraction cell_name)                                                                                                                                                                                                                                                           
                                                                let cell = {cname = cell_name; interactions = interactions} in 
                                                                (predicates, sync, species, cell::cells, switches, constraints)
                                                           | Species(a,b,c,d) -> 
                                                                let s = {name=a; reg_conds=Some(b); KO=c; FE=d; lVar = "L_" + a; KoVar="KO_"+a; FeVar="FE_"+a; NeedsActivator = false} in
                                                                (predicates, sync, s::species,cells, switches, constraints)
                                                           | Switch(c1, c2, b) -> 
                                                                let s = {cell1 = c1; cell2 = c2; condition = b} in
                                                                (predicates, sync, species,cells, s::switches, constraints)
                                                           | Assert(a,b,c,d,e) -> (predicates, sync, species, cells, switches, (a,b,c,d,e)::constraints)
                                                           ) ([],[],[],[],[],[]) spec
                                                          
 


let Translate (problem:Problem) =     
    
    let cst = new List<Fact>() in 
    //define an enumerator variable for each logic choice
    let lVars = Seq.map(fun s -> 
                    let vdef = VarDef.DeclareVar(System,BNat(17),s.lVar) in
                    let v = SysVar(s.lVar) in                    
                    let rc = 
                        match s.reg_conds with
                        | Some(rcl) -> rcl
                        | None -> [0..17]
                    let c = LOr(Seq.map(fun x -> BTerm(BComp(Eq(NTerm(NVar(v)),NTerm(NConst(x)))))) rc) //allowed logics only                            
                    cst.Add({expr=c; info=Some("allowed logic choices for " + s.name)});
                    s.lVar, vdef) problem.species in 

    //initialize FE and KO (perturbation) variables
    let pVars = Seq.fold(fun acc s -> 
        let a = if s.KO then Seq.singleton (s.KoVar,VarDef.DeclareVar(Scope.State,Bool,s.KoVar)) else Seq.empty in
        let b = if s.FE then Seq.singleton (s.FeVar,VarDef.DeclareVar(Scope.State,Bool,s.FeVar)) else Seq.empty in
        acc |> Seq.append a |> Seq.append b) Seq.empty problem.species in

 
    //initialize the state variables 
    // species
    let sVars = Seq.map(fun s -> (s.name,VarDef.DeclareVar(Scope.State,Bool,s.name))) problem.species in
    // cells
    let cVars = Seq.map(fun c -> (c.cname,VarDef.DeclareVar(Scope.State,Bool,c.cname))) problem.cells in
    
     // a map from cell name to interactions 
    let iMap = Map.ofSeq (Seq.map(fun c -> (c.cname, c.interactions)) problem.cells) in 
    let allInteractionsSeq =  Seq.map(fun c -> c.interactions) problem.cells in  
    let allInteractions = (Seq.reduce(fun a b -> Seq.append a b) allInteractionsSeq)  |> Seq.filter(fun i -> not i.definite) in  

    //Define a boolean system variable for each interaction (per cell)
    let iVars = Seq.map(fun i -> i.var,VarDef.DeclareUniqueVar(System,Bool,i.var)) allInteractions 

    // Define a fixed boolean variable for async cases 
    let fVarName = problem.fixedLabel
    let fVar = [fVarName, VarDef.DeclareVar(Scope.State,Bool,fVarName)]   
    //create a map of allowed switches 
    let all = Map.ofSeq (Seq.map(fun c-> c.cname, BTerm(BConst(true))) problem.cells)
    let cellNames = Seq.map(fun c-> c.cname) problem.cells
    let mutable mswitchMap = Map.ofSeq (Seq.map(fun c -> (c.cname, Map.empty )) problem.cells) in
    for s in problem.switches do mswitchMap <- mswitchMap.Add(s.cell1, (mswitchMap.[s.cell1].Add (s.cell2 , s.condition)));
    for c in cellNames do if Map.isEmpty mswitchMap.[c] then mswitchMap <- mswitchMap.Add(c, all);
    let switchMap = mswitchMap;
    

    //helper functions for updates - give the name of the cell and the name of the gene 
    let Activators s cellname = Seq.filter(fun i -> i.target=s && i.definite && i.positive) iMap.[cellname] in //INSTEAD OF: problem.interactions in 
    let Repressors s cellname = Seq.filter(fun i -> i.target=s && i.definite && not i.positive) iMap.[cellname]  in//problem.interactions in 
    let optActivators s cellname = Seq.filter(fun i -> i.target=s && (not i.definite) && i.positive) iMap.[cellname]  in //problem.interactions in
    let optRepressors s cellname = Seq.filter(fun i -> i.target=s && (not i.definite) && not i.positive) iMap.[cellname]  in //problem.interactions in
    
    let NotInducible s cellname = 
        if not (Seq.isEmpty(Activators s cellname)) then BTerm(BConst(false))
        else
          let A = optActivators s cellname in
          if Seq.isEmpty A then BTerm(BConst(true))
            else Not(LOr(Seq.map(fun i -> BTerm(BVar(SysVar(i.var)))) A))                
    let NotRepressible s cellname = 
        if not (Seq.isEmpty(Repressors s cellname)) then BTerm(BConst(false))
        else
          let R = optRepressors s cellname in
          if Seq.isEmpty R then BTerm(BConst(true))
            else Not(LOr(Seq.map(fun i -> BTerm(BVar(SysVar(i.var)))) R))
    let ActivatorAbsent s cellname = 
        let cls = Seq.map(fun i -> Not(BTerm(BVar(AbsStateVar(-1,i.source))))) (Activators s cellname)  //default activator is absent
                    |> Seq.append  (Seq.map(fun i -> And(BTerm(BVar(SysVar(i.var))),Not(BTerm(BVar(AbsStateVar(-1,i.source)))))) (optActivators s cellname)) //optional activator is selected but absent
            in
        if Seq.isEmpty cls then BTerm(BConst(false)) else LOr(cls) in
    let ActivatorPresent s cellname= 
        let cls = Seq.map(fun i -> BTerm(BVar(AbsStateVar(-1,i.source)))) (Activators s cellname)  //default activator is absent
                    |> Seq.append  (Seq.map(fun i -> And(BTerm(BVar(SysVar(i.var))),BTerm(BVar(AbsStateVar(-1,i.source))))) (optActivators s cellname)) //optional activator is selected but absent
            in
        if Seq.isEmpty cls then BTerm(BConst(false)) else LOr(cls) in
    let RepressorAbsent s cellname= 
        let cls = Seq.map(fun i -> Not(BTerm(BVar(AbsStateVar(-1,i.source))))) (Repressors s cellname)  //default activator is absent
                    |> Seq.append  (Seq.map(fun i -> And(BTerm(BVar(SysVar(i.var))),Not(BTerm(BVar(AbsStateVar(-1,i.source)))))) (optRepressors s cellname)) //optional activator is selected but absent
            in
        if Seq.isEmpty cls then BTerm(BConst(false)) else LOr(cls) in
    let RepressorPresent s cellname= 
        let cls = Seq.map(fun i -> BTerm(BVar(AbsStateVar(-1,i.source)))) (Repressors s cellname)  //default activator is absent
                    |> Seq.append  (Seq.map(fun i -> And(BTerm(BVar(SysVar(i.var))),BTerm(BVar(AbsStateVar(-1,i.source))))) (optRepressors s cellname)) //optional activator is selected but absent
            in
        if Seq.isEmpty cls then BTerm(BConst(false)) else LOr(cls) in

    //define the logic statements
    let SomeButNotAllActivatorsPresent s cellname= And(ActivatorAbsent s cellname, ActivatorPresent s cellname) in // Not all activators present, but at least one
    let NoActivatorsPresent s cellname= And(ActivatorAbsent s cellname, Not(ActivatorPresent s cellname)) in  // z3.MkAnd(Inducible, z3.MkNot(z3.MkOr(ActivatorPresent)))
    let AllActivatorsPresent s cellname= And(Not(ActivatorAbsent s cellname), ActivatorPresent s cellname) in // All of the activators are present
       
    let SomeButNotAllRepressorsPresent s cellname= And(RepressorAbsent s cellname, RepressorPresent s cellname) in    // Not all repressors present, but at least one
    let NoRepressorsPresent s cellname = And(RepressorAbsent s cellname, Not(RepressorPresent s cellname)) in   // None of the repressors are present (NB. assumes that the gene is repressible)
    let AllRepressorsPresent s cellname = And(Not(RepressorAbsent s cellname), RepressorPresent s cellname) in // All repressors present
                
    let Updates s cellname= 
        [| And(NotRepressible s cellname, AllActivatorsPresent s cellname); //logic 0
            And(NotRepressible s cellname, Or(SomeButNotAllActivatorsPresent s cellname, AllActivatorsPresent s cellname)); //logic 1
            Or(And(NoRepressorsPresent s cellname, Or(SomeButNotAllActivatorsPresent s cellname, AllActivatorsPresent s cellname)), And(NotRepressible s cellname, AllActivatorsPresent s cellname)); //logic 2
            And(Or(NoRepressorsPresent s cellname, NotRepressible s cellname), Or(SomeButNotAllActivatorsPresent s cellname, AllActivatorsPresent s cellname)); //logic 3
            And(AllActivatorsPresent s cellname, Not(AllRepressorsPresent s cellname)); //logic 4
            Or(And(NotRepressible s cellname, SomeButNotAllActivatorsPresent s cellname), And(AllActivatorsPresent s cellname, Not(AllRepressorsPresent s cellname)));  //logic 5
            Or(And(SomeButNotAllActivatorsPresent s cellname, Or(NoRepressorsPresent s cellname, SomeButNotAllRepressorsPresent s cellname)), And(AllActivatorsPresent s cellname, Not(AllRepressorsPresent s cellname)));  //logic 6
            And(Or(SomeButNotAllActivatorsPresent s cellname, AllActivatorsPresent s cellname), Not(AllRepressorsPresent s cellname));  //logic 7
            AllActivatorsPresent s cellname; //logic 8
            Or(AllActivatorsPresent s cellname, And(SomeButNotAllActivatorsPresent s cellname, NotRepressible s cellname));  //logic 9
            Or(AllActivatorsPresent s cellname, And(SomeButNotAllActivatorsPresent s cellname, NoRepressorsPresent s cellname)); //logic 10
            Or(AllActivatorsPresent s cellname, And(SomeButNotAllActivatorsPresent s cellname, Or(NotRepressible s cellname, NoRepressorsPresent s cellname)));  //logic 11
            Or(AllActivatorsPresent s cellname, And(SomeButNotAllActivatorsPresent s cellname, Or(NoRepressorsPresent s cellname, SomeButNotAllRepressorsPresent s cellname)));  //logic 12
            Or(AllActivatorsPresent s cellname, And(SomeButNotAllActivatorsPresent s cellname, Not(AllRepressorsPresent s cellname)));  //logic 13
            Or(AllActivatorsPresent s cellname, And(SomeButNotAllActivatorsPresent s cellname, Not(NotRepressible s cellname)));  //logic 14
            Or(SomeButNotAllActivatorsPresent s cellname, AllActivatorsPresent s cellname);  //logic 15
            And(SomeButNotAllRepressorsPresent s cellname, NotInducible s cellname); //logic 16 (for TCF3 and MEKERK - part of signaling and activated by default)
            And(NoRepressorsPresent s cellname, NotInducible s cellname); //logic 17 (for TCF3 and MEKERK - part of signaling and activated by default)
        |] in

      
      // species update - also conditional on the cell
       let UpdateFn (s:Species) (c:Cell) =
        let UpdateFns = Updates s.name c.cname in

        let rc = 
            match s.reg_conds with
            | Some(rcl) -> rcl
            | None -> [0..17]

        let expr = match (s.KO,s.FE) with
                    | true,true -> LAnd(Seq.map(fun i -> Imp(BTerm(BComp(Eq(NTerm(NVar(SysVar(s.lVar))),NTerm(NConst(i))))), 
                                                            Or(And(UpdateFns.[i], Not(BTerm(BVar(AbsStateVar(-1, s.KoVar))))),
                                                             BTerm(BVar(AbsStateVar(-1, s.FeVar)))))) rc)
                    | true, false -> LAnd(Seq.map(fun i -> Imp(BTerm(BComp(Eq(NTerm(NVar(SysVar(s.lVar))),NTerm(NConst(i))))), 
                                                            And(UpdateFns.[i], Not(BTerm(BVar(AbsStateVar(-1, s.KoVar))))))) rc)                                  
                    | false, true -> LAnd(Seq.map(fun i -> Imp(BTerm(BComp(Eq(NTerm(NVar(SysVar(s.lVar))),NTerm(NConst(i))))), 
                                                            Or(UpdateFns.[i],BTerm(BVar(AbsStateVar(-1, s.FeVar)))))) rc)                                  
                    | false,false -> LAnd(Seq.map(fun i -> Imp(BTerm(BComp(Eq(NTerm(NVar(SysVar(s.lVar))),NTerm(NConst(i))))), UpdateFns.[i])) rc)
         in
         UpdateRule.Create(s.name,BExpr(expr))
  

    
    let cellUpdate (c1) (c2) = 
       List.toSeq[UpdateRule.Create(c2, BExpr(BTerm(BVar(AbsStateVar(-1, c1)))));
        UpdateRule.Create(c1, BExpr(Not(BTerm(BVar(AbsStateVar(-1, c1))))))]
    
    let cellCondition (c:Cell, expr:BExpr) = 
        if (expr.ToString().Equals("True")) then LAnd(Seq.map(fun o ->  if (o.cname = c.cname) then Beq(BTerm(BVar(AbsStateVar(-1, o.cname))), BTerm(BConst(true))) else Beq(BTerm(BVar(AbsStateVar(-1, o.cname))), BTerm(BConst(false)))) problem.cells)  
                                            else LAnd(Seq.append (Seq.map(fun o ->  if (o.cname = c.cname) then Beq(BTerm(BVar(AbsStateVar(-1, o.cname))), BTerm(BConst(true))) else Beq(BTerm(BVar(AbsStateVar(-1, o.cname))), BTerm(BConst(false)))) problem.cells) [expr]) 
    
    let cellConditionFixed(c:Cell, expr:BExpr, fixedStateVar:string, isFixed:bool) = 
        if (expr.ToString().Equals("True")) then LAnd(Seq.append (Seq.map(fun o ->  if (o.cname = c.cname) then Beq(BTerm(BVar(AbsStateVar(-1, o.cname))), BTerm(BConst(true))) else Beq(BTerm(BVar(AbsStateVar(-1, o.cname))), BTerm(BConst(false)))) problem.cells) [Beq(BTerm(BVar(AbsStateVar(-1, fixedStateVar))), BTerm(BConst(isFixed)))] )  
                                            else LAnd( (Seq.map(fun o ->  if (o.cname = c.cname) then Beq(BTerm(BVar(AbsStateVar(-1, o.cname))), BTerm(BConst(true))) else Beq(BTerm(BVar(AbsStateVar(-1, o.cname))), BTerm(BConst(false)))) problem.cells) |> Seq.append [expr] |> Seq.append [Beq(BTerm(BVar(AbsStateVar(-1, fixedStateVar))), BTerm(BConst(isFixed)))] ) 

    let initialCellCondition = LAnd(Seq.map(fun c ->  Beq(BTerm(BVar(AbsStateVar(-1, c.cname))), BTerm(BConst(true)))) problem.cells) 
    let initialCellUpdate (c) =  Seq.map(fun o -> if (c.cname = o.cname) then UpdateRule.Create(o.cname, BExpr(BTerm(BVar(AbsStateVar(-1, c.cname))))) else UpdateRule.Create(o.cname, BExpr(Not(BTerm(BVar(AbsStateVar(-1, c.cname))))))) problem.cells
    
    let syncUpdatePre = [for c in problem.cells do let others = switchMap.[c.cname] in
                                                   for o in others -> if o.Key <> c.cname then Update.Create(Option.Some (cellCondition(c, o.Value)), Seq.append (cellUpdate c.cname o.Key) (Seq.map(fun s -> UpdateFn s c) problem.species)) else Update.Create(Option.Some (cellCondition(c, o.Value)), (Seq.map(fun s -> UpdateFn s c) problem.species))] 
    let syncUpdate = if (Seq.length problem.cells > 1) then (List.append syncUpdatePre [for c in problem.cells -> Update.Create(Option.Some (initialCellCondition), Seq.append (initialCellUpdate(c)) (Seq.map(fun s -> UpdateFn s c) problem.species)) ]) else syncUpdatePre 
    

    let nonFixedAsyncUpdate = [for c in problem.cells do let others = switchMap.[c.cname] in
                                                          for o in others do for s in problem.species -> if o.Key <> c.cname then Update.Create(Option.Some (cellConditionFixed(c, o.Value, fVarName, false)), Seq.append (cellUpdate c.cname o.Key) [UpdateFn s c]) else Update.Create(Option.Some(cellConditionFixed(c, o.Value, fVarName, false)), [UpdateFn s c])]  
    let transientAsyncUpdate = [for c in problem.cells do let others = switchMap.[c.cname] in
                                                          for o in others do for s in problem.species -> if o.Key <> c.cname then 
                                                                                                         Update.Create(Option.Some (cellConditionFixed(c, o.Value, fVarName, false)),
                                                                                                           (cellUpdate c.cname o.Key) |> Seq.append [UpdateFn s c] |> Seq.append [UpdateRule.Create(fVarName, BExpr(BTerm(BConst(true))))]) else Update.Create(Option.Some(cellConditionFixed(c, o.Value, fVarName, false)), Seq.append [UpdateFn s c] [UpdateRule.Create(fVarName, BExpr(BTerm(BConst(true))))])]  
    
    let fixedAsyncUpdate =  [for c in problem.cells do let others = switchMap.[c.cname] in for o in others -> if o.Key <> c.cname then Update.Create(Option.Some (cellConditionFixed(c, o.Value, fVarName, true)), Seq.append (cellUpdate c.cname o.Key) (Seq.map(fun s -> UpdateFn s c) problem.species)) else Update.Create(Option.Some (cellConditionFixed(c, o.Value, fVarName, true)), (Seq.map(fun s -> UpdateFn s c) problem.species))] 
         
    let asyncUpdatePre = nonFixedAsyncUpdate |> List .append fixedAsyncUpdate |> List .append transientAsyncUpdate
    let asyncUpdate = if (Seq.length problem.cells > 1) then (List.append asyncUpdatePre   [for c in problem.cells do for s in problem.species -> Update.Create(Option.Some (initialCellCondition), Seq.append (initialCellUpdate(c)) [UpdateFn s c])]) else asyncUpdatePre 
           
    
  
    let update = if problem.sync then syncUpdate else asyncUpdate

    //format the output as a ReasoningEngine problem
    let cellLabels = Seq.toList (Seq.map(fun c1 ->  "$"+c1.cname ,LAnd(Seq.map(fun c2 ->  if (c2.cname = c1.cname) then Beq(BTerm(BVar(AbsStateVar(0, c2.cname))), BTerm(BConst(true))) else Beq(BTerm(BVar(AbsStateVar(0, c2.cname))), BTerm(BConst(false)))) problem.cells)) problem.cells)
    let cellSW = Seq.map( fun c ->  let others = switchMap.[c.cname] in 
                                    let keys = [ for o in others -> o.Key] in
                                    c.cname, keys) problem.cells |> Map.ofSeq 
    let cellSWLabels  = Seq.toList (Seq.map(fun c1 ->  "$"+c1.cname+"_SW", 
                                                        LOr(Seq.map(fun o -> LAnd( Seq.map(fun c2 ->  if (c2.cname = o) then Beq(BTerm(BVar(AbsStateVar(0, c2.cname))), BTerm(BConst(true))) else Beq(BTerm(BVar(AbsStateVar(0, c2.cname))), BTerm(BConst(false)))) problem.cells)) cellSW.[c1.cname])) problem.cells)
    let predicates = problem.labels |> Seq.append cellLabels |> Seq.append cellSWLabels|> Map.ofSeq in //|> Map.map(fun n e -> Arbitrary(e)) in
 
    
    let vars = sVars |> Seq.append cVars |> Seq.append fVar |> Seq.append iVars |> Seq.append lVars |> Seq.append pVars |> Map.ofSeq in
    let spec = {        predicates = predicates; 
        facts= cst |> Seq.map(fun x -> x.expr);
        observations = problem.constraints |> Seq.map(fun x -> x.expr, if x.info.IsSome then x.info.Value else "")
        } in
    {system=DSystem.NewSystem(vars,update); constraints = spec; settings = Microsoft.Research.ReasoningEngine.Settings.defaultSettings}     
        
let ParseAST str = 
    let (labels, sync, species, cells, switches, constraints) = str |> ParseText |> SplitFields in
    let syncVal = if List.length sync = 0 then true else sync.[0] in  
    let fVarName = "fixed"
    let constraints = if syncVal then Seq.map(fun (path, step, exp:BExpr, description, asyncFixed) -> {expr = (exp.Init path step); info = Some(description)}) constraints else  
                        let paths =  Seq.distinct (Seq.map(fun (path, step, exp:BExpr, description, fixedAsync)-> path ) constraints) 
                        let fixedPaths = constraints |> Seq.filter (fun (path, step, exp:BExpr, description, fixedAsync) -> fixedAsync = true) 
                                                      |> Seq.map(fun (path, step, exp:BExpr, description, fixedAsync) -> path)
                        
                        let mutable mlastTimes = (Seq.map(fun (path) -> (path,0)) paths) |> Map.ofSeq
                        for (path, step, exp:BExpr, description, fixedAsync) in constraints do if (step > mlastTimes.[path]) then mlastTimes <- mlastTimes.Add(path, step)
                        let isSamePath (x:string, p:string) =  x.Equals(p)
                        let lastTimes = mlastTimes 
                        let AsyncConstraints = Seq.ofList [for p in paths do let lastTime = lastTimes.[p] in
                                                                             for t in 0 .. lastTime ->  if (t < (lastTime-1)) then {expr = ((Beq(BTerm(BVar(AbsStateVar(0, fVarName))), BTerm(BConst(false)))).Init p t); info = None}
                                                                                                        else {expr = ((Beq(BTerm(BVar(AbsStateVar(0, fVarName))), BTerm(BConst(Seq.exists (fun elem -> elem = p) fixedPaths)))).Init p t); info = None}]
                                                                                                                                                                  
                        Seq.append ( Seq.map(fun (path, step, exp:BExpr, description, asyncFixed) -> {expr = (exp.Init path step); info = Some(description)}) constraints) AsyncConstraints in 
    {labels=labels; sync=syncVal; species=species; cells=cells; switches = switches; constraints=constraints; fixedLabel = fVarName}    

let Parse str = 
    ParseAST str
    |> Translate