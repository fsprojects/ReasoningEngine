module Microsoft.Research.REIN.ReinMain
open Microsoft.Research.ReasoningEngine.Var
open Microsoft.Research.ReasoningEngine.Constraint
open Microsoft.Research.ReasoningEngine.Model
open Microsoft.Research.ReasoningEngine.Dynamics
open System.Collections.Generic
open Parser

type Interaction = {
    source: string;
    target: string;
    positive: bool;
    definite: bool;
    var: string;
    }
type Fact = {
    expr: BExpr;
    info: string;
    }
type Species = {
    name: string;
    logics: int list;
    KO: bool;
    FE: bool;
    lVar: string;
    KoVar: string;
    FeVar: string;
    }

type Problem = {
    species:seq<Species>;
    interactions: seq<Interaction>;
    constraints: seq<Fact>;
    }


let ParseAST str = 
        let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<char>.FromString(str)   in                                                     
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
            raise (new System.Exception(System.String.Format("RE:IN({0},{1}):error : parsing failed at line {0}, column {1}: {2}",line+1,column,e.Message)));    


let SplitFields spec = 
    Seq.fold(fun (species, interactions, constraints) x -> match x with 
                                                           | Assign -> (species, interactions, constraints) 
                                                           | State(a,b,c,d) -> (species,interactions,(a,b,c,d)::constraints)
                                                           | Interaction(a,b,c,d) -> 
                                                                let name = (if c then "Pos_" else "Neg_") + a + "_" + b in 
                                                                let interaction = {source=a; target=b; positive=c; definite=d; var=name} in
                                                                (species,interaction::interactions,constraints)
                                                           | Species(a,b,c,d) -> 
                                                                let s = {name=a; logics=b; KO=c; FE=d; lVar = "L_" + a; KoVar="KO_"+a; FeVar="FE_"+a} in
                                                                (s::species,interactions,constraints)
                                                           ) ([],[],[]) spec
    
    
let Translate (problem:Problem) = 
    //Define a boolean system variable for each possible interaction
    let cst = new List<Fact>();
    let iVars = Seq.map(fun i -> i.var,VarDef(System,Bool,i.var)) problem.interactions in
    //define an enumerator variable for each logic choice
    let lVars = Seq.map(fun s -> 
                    let vdef = VarDef(System,BNat(15),s.name) in
                    let v = SysVar(s.lVar) in
                    let c = Seq.map(fun x -> BTerm(Eq(NVar(v),NConst(x)))) s.logics //allowed logics only
                            |> Seq.reduce(fun a b -> Or(a,b)) in
                    cst.Add({expr=c; info="allowed logic choices for " + s.name});
                    s.lVar, vdef) problem.species in
    
    //initialize FE and KO (perturbation) variables
    let pVars = Seq.fold(fun acc s -> 
        let a = if s.KO then Seq.singleton (s.KoVar,VarDef(System,Bool,s.KoVar)) else Seq.empty in
        let b = if s.FE then Seq.singleton (s.FeVar,VarDef(System,Bool,s.FeVar)) else Seq.empty in
        acc |> Seq.append a |> Seq.append b) Seq.empty problem.species in


    //initialize the state variables
    let sVars = Seq.map(fun s -> (s.name,VarDef(Scope.State,Bool,s.name))) problem.species in
    
    //helper functions for updates
    let Activators s = Seq.filter(fun i -> i.target=s && i.definite && i.positive) problem.interactions in
    let Repressors s = Seq.filter(fun i -> i.target=s && i.definite && not i.positive) problem.interactions in
    let optActivators s = Seq.filter(fun i -> i.target=s && (not i.definite) && i.positive) problem.interactions in
    let optRepressors s = Seq.filter(fun i -> i.target=s && (not i.definite) && not i.positive) problem.interactions in
    let NotInducible s = 
        if not (Seq.isEmpty(Activators s)) then BConst(false) 
        else
          let A = optActivators s in
          if Seq.isEmpty A then BConst(true)
          else
              Seq.map(fun i -> BVar(SysVar(i.var))) A
              |> Seq.reduce(fun a b -> Or(a,b))
              |> Not in
    let NotRepressible s = 
        if not (Seq.isEmpty(Repressors s)) then BConst(false) 
        else
          let R = optRepressors s in
          if Seq.isEmpty R then BConst(true)
          else
              Seq.map(fun i -> BVar(SysVar(i.var))) R
              |> Seq.reduce(fun a b -> Or(a,b))
              |> Not in
      let ActivatorAbsent s = 
            let cls = Seq.map(fun i -> Not(BVar(AbsStateVar(-1,i.source)))) (Activators s)  //default activator is absent
                      |> Seq.append  (Seq.map(fun i -> And(BVar(SysVar(i.var)),Not(BVar(AbsStateVar(-1,i.source))))) (optActivators s)) //optional activator is selected but absent
            in
            if Seq.isEmpty cls then BConst(false) else Seq.reduce(fun a b -> Or(a,b)) cls in
      let ActivatorPresent s = 
            let cls = Seq.map(fun i -> BVar(AbsStateVar(-1,i.source))) (Activators s)  //default activator is absent
                      |> Seq.append  (Seq.map(fun i -> And(BVar(SysVar(i.var)),BVar(AbsStateVar(-1,i.source)))) (optActivators s)) //optional activator is selected but absent
            in
            if Seq.isEmpty cls then BConst(false) else Seq.reduce(fun a b -> Or(a,b)) cls in
      let RepressorAbsent s = 
            let cls = Seq.map(fun i -> Not(BVar(AbsStateVar(-1,i.source)))) (Repressors s)  //default activator is absent
                      |> Seq.append  (Seq.map(fun i -> And(BVar(SysVar(i.var)),Not(BVar(AbsStateVar(-1,i.source))))) (optRepressors s)) //optional activator is selected but absent
            in
            if Seq.isEmpty cls then BConst(false) else Seq.reduce(fun a b -> Or(a,b)) cls in
      let RepressorPresent s = 
            let cls = Seq.map(fun i -> BVar(AbsStateVar(-1,i.source))) (Repressors s)  //default activator is absent
                      |> Seq.append  (Seq.map(fun i -> And(BVar(SysVar(i.var)),BVar(AbsStateVar(-1,i.source)))) (optRepressors s)) //optional activator is selected but absent
            in
            if Seq.isEmpty cls then BConst(false) else Seq.reduce(fun a b -> Or(a,b)) cls in

        //define the logic statements
       let SomeButNotAllActivatorsPresent s = And(ActivatorAbsent s, ActivatorPresent s) in // Not all activators present, but at least one
       let NoActivatorsPresent s = And(ActivatorAbsent s, Not(ActivatorPresent s)) in  // z3.MkAnd(Inducible, z3.MkNot(z3.MkOr(ActivatorPresent)))
       let AllActivatorsPresent s = And(Not(ActivatorAbsent s), ActivatorPresent s) in // All of the activators are present
       
       let SomeButNotAllRepressorsPresent s = And(RepressorAbsent s, RepressorPresent s) in    // Not all repressors present, but at least one
       let NoRepressorsPresent s = And(RepressorAbsent s, Not(RepressorPresent s)) in   // None of the repressors are present (NB. assumes that the gene is repressible)
       let AllRepressorsPresent s = And(Not(RepressorAbsent s), RepressorPresent s) in // All repressors present
                
       let Updates s = 
            [| And(NotRepressible s, AllActivatorsPresent s); //logic 0
               And(NotRepressible s, Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s)); //logic 1
               Or(And(NoRepressorsPresent s, Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s)), And(NotRepressible s, AllActivatorsPresent s)); //logic 2
               And(Or(NoRepressorsPresent s, NotRepressible s), Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s)); //logic 3
               And(AllActivatorsPresent s, Not(AllRepressorsPresent s)); //logic 4
               Or(And(NotRepressible s, SomeButNotAllActivatorsPresent s), And(AllActivatorsPresent s, Not(AllRepressorsPresent s)));  //logic 5
               Or(And(SomeButNotAllActivatorsPresent s, Or(NoRepressorsPresent s, SomeButNotAllRepressorsPresent s)), And(AllActivatorsPresent s, Not(AllRepressorsPresent s)));  //logic 6
               And(Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s), Not(AllRepressorsPresent s));  //logic 7
               AllActivatorsPresent s; //logic 8
               Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, NotRepressible s));  //logic 9
               Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, NoRepressorsPresent s)); //logic 10
               Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Or(NotRepressible s, NoRepressorsPresent s)));  //logic 11
               Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Or(NoRepressorsPresent s, SomeButNotAllRepressorsPresent s)));  //logic 12
               Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Not(AllRepressorsPresent s)));  //logic 13
               Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Not(NotRepressible s)));  //logic 14
               Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s);  //logic 15
               And(SomeButNotAllRepressorsPresent s, NotInducible s); //logic 16 (for TCF3 and MEKERK - part of signaling and activated by default)
               And(NoRepressorsPresent s, NotInducible s); //logic 17 (for TCF3 and MEKERK - part of signaling and activated by default)
            |] in


            //NEED TO CONSTRUCTTHE OVERALL UPDATE FN
       let UpdateFn (s:Species) =
        let UpdateFns = Updates s.name in
        let expr = match (s.KO,s.FE) with
                    | true,true -> Seq.map(fun i -> Imp(BTerm(Eq(NVar(SysVar(s.lVar)),NConst(i))), 
                                                        Or(And(UpdateFns.[i], Not(BVar(SysVar(s.KoVar)))),
                                                           BVar(SysVar(s.FeVar))))) [0..17] 
                                  |> Seq.reduce(fun a b -> And(a,b))
                    | true, false -> Seq.map(fun i -> Imp(BTerm(Eq(NVar(SysVar(s.lVar)),NConst(i))), 
                                                          And(UpdateFns.[i], Not(BVar(SysVar(s.KoVar)))))) s.logics
                                  |> Seq.reduce(fun a b -> And(a,b))
                    | false, true -> Seq.map(fun i -> Imp(BTerm(Eq(NVar(SysVar(s.lVar)),NConst(i))), 
                                                        Or(UpdateFns.[i],BVar(SysVar(s.FeVar))))) s.logics
                                  |> Seq.reduce(fun a b -> And(a,b))
                    | false,false -> Seq.map(fun i -> Imp(BTerm(Eq(NVar(SysVar(s.lVar)),NConst(i))), UpdateFns.[i])) s.logics 
                                     |> Seq.reduce(fun a b -> And(a,b)) in
         UpdateRule(s.name,BExpr(expr))


    //synchronous updates
    let update = [Update(None,Seq.map(fun s -> UpdateFn s) problem.species)] in

    //format the output as a ReasoningEngine problem
    let vars = sVars |> Seq.append iVars |> Seq.append lVars |> Seq.append pVars |> Map.ofSeq in
    let spec = {labels = Map.empty; constraints=Seq.append(problem.constraints) (cst|> ResizeArray.toSeq) |> Seq.map(fun x -> BExpr(x.expr))} in //NOTE: descriptions are currently ignored
    let model = Model(vars,update) in
    {model=model; spec = spec; options = Microsoft.Research.ReasoningEngine.Settings.defaultSettings}   
   
    
        
let Parse str = 
    let (species, interactions, constraints) = str |> ParseAST |> SplitFields in
    let constraints = Seq.map(fun (path, step, exp:BExpr, description) -> {expr = (exp.Init path step); info = description}) constraints in
    {species=species; interactions=interactions; constraints=constraints}
    |> Translate