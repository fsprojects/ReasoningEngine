module Microsoft.Research.ReasoningEngine.Model
open System.Collections.Generic
open Var
open Constraint
open Settings


/// <summary>
///  A reasoning engine DSystem (dynamical system) contains a map of named variable definitions and a set of update rules.
/// </summary>
type DSystem = {
    stateVars  : Map<string,VarDef>;
    pathVars   : Map<string,VarDef>;
    systemVars : Map<string,VarDef>;
    updates: seq<Dynamics.Update>    
    } with     
    override this.ToString() = 
        Seq.map(fun x -> x.ToString()) this.updates
        |> Seq.append(Seq.map(fun (_,x) -> x.ToString()) (this.varDefs|> Map.toSeq))
        |> Seq.reduce(fun a b -> a + "\r\n" + b)

    member this.ToLatex() =       
        //system parameters
        let syslx = 
            let sysvars = this.systemVars |> Map.toSeq  in
            if Seq.isEmpty sysvars then @"\[ S = []\]"
            else
                let sysvars = sysvars|> Seq.map(fun (_,v:VarDef) -> v.ToLatex()) |> Seq.reduce(fun a b -> a + @"\\" + b) in
                @"\[ S = \left[\begin{array}{c}"  + sysvars + @"\end{array}\right] \]" in
    
        //path parameters
        let pathlx = 
            let pathvars = this.pathVars |> Map.toSeq  in
            if Seq.isEmpty pathvars then @"\[ P = []\]"
            else
                let pathvars = pathvars |> Seq.map(fun (_,v:VarDef) -> v.ToLatex()) |> Seq.reduce(fun a b -> a + @"\\" + b) in
                @"\[ P = \left[\begin{array}{c}"  + pathvars + @"\end{array}\right] \]" in

        //state variables
        let statelx = 
            let statevars = this.stateVars |> Map.toSeq 
            if Seq.isEmpty statevars then @"\[ X = []\]"
            else
                let statevars = statevars |> Seq.map(fun (_,v:VarDef) -> v.ToLatex()) |> Seq.reduce(fun a b -> a + @"\\" + b) in        
                @"\[ X = \left[\begin{array}{c}"  + statevars + @"\end{array}\right] \]" in

        let varslx = syslx + pathlx + statelx in

        //update rulse      
        let updates = 
            if Seq.isEmpty this.updates then @"(no dynamics) " else
            Seq.map(fun (x:Dynamics.Update) -> x.ToLatex()) this.updates |> Seq.reduce(fun a b -> a + @"\\" + b)
        let updateslx = @"\[\forall p, k\;.\; \left\{\begin{array}{c}" + updates + @"\end{array}\right.\]" in
        
        varslx + updateslx



    static member EmptySystem = {stateVars = Map.empty; pathVars=Map.empty; systemVars = Map.empty; updates = Seq.empty}    
    member this.DeclareStateVar(name,t) = {this with stateVars = this.stateVars.Add(name,VarDef.DeclareVar(State,t,name))}
    member this.DeclareSystemVar(name,t) = {this with systemVars = this.systemVars.Add(name,VarDef.DeclareVar(System,t,name))}
    member this.DeclarePathVar(name,t) = {this with pathVars = this.pathVars.Add(name,VarDef.DeclareVar(Path,t,name))}        
    member this.DeclareUniqueStateVar(name,t) = {this with stateVars = this.stateVars.Add(name,VarDef.DeclareUniqueVar(State,t,name))}
    member this.DeclareUniqueSystemVar(name,t) = {this with systemVars = this.systemVars.Add(name,VarDef.DeclareUniqueVar(System,t,name))}
    member this.DeclareUniquePathVar(name,t) = {this with pathVars = this.pathVars.Add(name,VarDef.DeclareUniqueVar(Path,t,name))}  
    member this.DefineUpdate(update) = {this with updates = Seq.append(this.updates) [update]}

    //these functions are left for backwards compatability: Depricate soon?
    member this.DeclareVar(name,var,unique) = 
        match var.scope with
        | System -> if unique then this.DeclareUniqueSystemVar(name,var.t) else this.DeclareSystemVar(name,var.t)
        | State -> if unique then this.DeclareUniqueStateVar(name,var.t) else this.DeclareStateVar(name,var.t)
        | Path -> if unique then this.DeclareUniquePathVar(name,var.t) else this.DeclarePathVar(name,var.t)                
    static member NewSystem(V,U) = 
        let sys = {DSystem.EmptySystem with updates=U}
        Seq.fold(fun (s:DSystem) (_,v) -> s.DeclareVar(v.name,v,v.unique)) sys (V |> Map.toSeq)    
    member this.varDefs = 
        let state_seq = this.stateVars |> Map.toSeq
        let path_seq = this.pathVars |> Map.toSeq
        let system_seq = this.systemVars |> Map.toSeq
        Seq.append(state_seq) (Seq.append(path_seq) system_seq) |> Map.ofSeq                


//type Predicate = 
//    |BAssignments of Map<Var,BExpr> //conjunction of assignment variable
//    |NAssignments of Map<Var,NExpr> //conjunction of assignment variable
//    |Arbitrary of BExpr
//    member this.Expr = 
//        match this with
//        |BAssignments(vm) -> LAnd(Seq.map(fun (v,e) -> Beq(BVar(v),e)) (Map.toSeq vm))
//        |NAssignments(vm) -> LAnd(Seq.map(fun (v,e) -> BTerm(Eq(NVar(v),e))) (Map.toSeq vm))
//        |Arbitrary(e) -> e
//    member this.Simplify = 
//        match this with
//        |BAssignments(vm) -> BAssignments(Map.map(fun n (e:BExpr) -> e.Simplify) vm)
//        |NAssignments(vm) -> NAssignments(Map.map(fun n (e:NExpr) -> e.Simplify) vm)
//        |Arbitrary(e) -> Arbitrary(e.Simplify)
//           
   
/// <summary>
/// The Constraints structure describes additional information about a model (e.g. experimental observations)
/// Currently, all constraints are Boolean, expected to evaluate to TRUE in correct models
/// </summary>
type Constraints = {
     predicates : Map<string, BExpr>;   //Map<string, Predicate>
     facts     : seq<BExpr>;            //hard constraints (e.g. imposed by formalism assumptions)
     observations: seq<BExpr * string>; //additional constraints (e.g. experimental observations)
     } with 
     override this.ToString() = 
        let pred_str = 
            if this.predicates.Count=0 then "" 
            else
                this.predicates 
                |> Map.toSeq 
                |> Seq.map (fun (n,e) -> "let " + n + " := {\n" + e.ToString() + "\n};")
                |> Seq.reduce(fun a b -> a + "\n" + b)
         in
         let toStr s = if (Seq.length s)=0 then "" else Seq.map(fun e -> e.ToString()) s |> Seq.reduce(fun a b -> a + "\n" + b) in
         let fact_str = toStr this.facts in
         let obs_str = toStr this.observations in                              
         "//Predicates:\n" + pred_str + "\n\n//Facts:\n" +  fact_str + "\n\n//Observations:\n" + obs_str

     member this.ToLatex() = 
        let pred_str = 
            if this.predicates.Count=0 then "" 
            else
                this.predicates 
                |> Map.toSeq 
                |> Seq.map (fun (n,e) -> "\\[" + n + "(p,k) :=" + e.ToLatex() + "\\]")
                |> Seq.reduce(fun a b -> a + "\n" + b)
         in
         let toLatex (s:seq<BExpr>) = if (Seq.length s)=0 then "" else Seq.map(fun (e:BExpr) -> "\\[ " + e.ToLatex() + "\\]") s |> Seq.reduce(fun a b -> a + "\\\\\n" + b) in
         let fact_str = toLatex this.facts in
         let obs_str = toLatex (this.observations |> Seq.map fst) in                              
         "\\\\\nPredicates:\\\\\n" + pred_str + "\n\\\\\\\\\nFacts:\n\\\\\n" +  fact_str + "\n\\\\\\\\\nObservations:\n\\\\\n" + obs_str         
     static member EmptyConstraints = {predicates = Map.empty; facts=Seq.empty; observations = Seq.empty}
     member this.AddPredicate(name,exp) = {this with predicates = this.predicates.Add(name,exp)}
     member this.AddFact(exp) = {this with facts = Seq.append(this.facts) [exp]} 
     member this.AddObservation(exp,description) = {this with observations = Seq.append(this.observations) [exp,description]} 
     member this.GetConstraints() = Seq.append(this.facts) (this.observations |> Seq.map fst)
                                                                           
/// <summary>
/// The Problem is a constrained model
/// </summary>                                                                                    
type Model = {
    system: DSystem;
    constraints: Constraints;
    settings: Options;
    } with
    override this.ToString() = "//REIL Model:\n\n" + this.system.ToString() + "\n\n//REIL Constraints:\n\n" + this.constraints.ToString()
    member this.ToLatex() = this.system.ToLatex() + this.constraints.ToLatex()//@"\emph{Model constraints are currently omitted}" //TODO: include model constraints
    static member EmptyModel = {system = DSystem.EmptySystem; constraints = Constraints.EmptyConstraints; settings = defaultSettings}
    static member NewModel(S) = {Model.EmptyModel with system=S}
    member this.Assert(exp) = {this with constraints = this.constraints.AddFact(exp)}
    member this.Observed(exp,desc) = {this with constraints = this.constraints.AddObservation(exp,desc)}
    member this.DeclareVar(name,var) = {this with system = this.system.DeclareVar(name,var,var.unique)}
    member this.DefineUpdate(update) = {this with system = this.system.DefineUpdate(update)}
    member this.Simplify() = 
        let new_facts = this.constraints.facts  |> Seq.map(fun c -> c.Simplify()) in
        let new_predicates = Map.map(fun n (e:BExpr) -> e.Simplify()) this.constraints.predicates in
        let new_observations = this.constraints.observations |> Seq.map(fun (e,d) -> e.Simplify(),d) in
        let new_constraints = {facts=new_facts; predicates=new_predicates; observations = new_observations} in
        let new_system = DSystem.NewSystem(this.system.varDefs, Seq.map(fun (u:Dynamics.Update) -> u.Simplify()) this.system.updates) in
        let problem = {this with constraints=new_constraints; system=new_system} in
        problem
    member this.Vars() = 
        let cstvars = 
            this.constraints.GetConstraints()
            |> Seq.map(fun e -> e.Vars()) //extract variables
            |> Seq.concat                 //merge the variables from all expressions
            |> Seq.distinct               //take the unique variables
        
        //If there are no constraints as part of the model, system variables are referenced in the constraints
        //Therefore, no solutions are identified, which causes problems
        //To avoid that, the system variables (which do not need to be instanitated to paths and/or timesteps) are added
        let sysvars = 
            this.system.systemVars
            |> Map.toSeq
            |> Seq.map(fun (v,_) -> Var.SysVar(v))
        
        Seq.append cstvars sysvars
        |> Seq.distinct
    member this.Negate() = 
        let neg_facts = 
            this.constraints.facts
            |> Seq.map(fun f -> Not(f))
            |> LOr
            |> Seq.singleton

        let neg_constraints = {this.constraints with facts = neg_facts}
        {this with constraints = neg_constraints}


let Parse str =         
    let ModelFromAST (spec:Parser.Spec) = 
        let statements = match spec with Parser.Spec e -> e         
        List.fold(fun (model:Model) s -> 
                    match s with
                    | Parser.Assert(e) -> model.Assert(e)
                    //| Parser.AssignLabel(n,e) -> ((n,e)::l,f,d,v,u)
                    //| Parser.Directive(dr) ->(l,f,dr::d,v,u)
                    | Parser.VarDecl(v) -> model.DeclareVar(v.name,v)
                    | Parser.UpdateDecl(u) -> model.DefineUpdate(u)
                ) Model.EmptyModel statements                       

    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString(str)                                                 
    try
        let ast = Parser.start Lexer.tokenize lexbuf
        ModelFromAST ast
    with e ->                                
        let pos = lexbuf.EndPos
        let line = pos.Line
        let column = pos.Column  
        raise (new System.Exception(System.String.Format("REIL({0},{1}):error : parsing failed at line {0}, column {1}.",line+1,column)))