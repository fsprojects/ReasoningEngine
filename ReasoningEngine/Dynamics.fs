module Microsoft.Research.ReasoningEngine.Dynamics
open Var
open Constraint
open System.Collections.Generic


type AssignmentRule = {
    var: string; //variable that gets assigned
    expr: Expr; //expression determining the updated value //FIXME: THIS SHOULD ALSO ALLOW BOOLEAN VARIABLE ASSIGNMENTS!
    } with    
    override this.ToString() = 
        "p[k]." + this.var + ":=" + this.expr.ToString()
    member this.ToLatex() = 
        "p[k]." + this.var + ":=" + this.expr.ToLatex()
    member this.Simplfy() = AssignmentRule.Create(this.var, this.expr.Simplify())
    static member Create(var,expr) = {var=var; expr=expr}

type RelationRule = {
    var: seq<string>; //set of variables that get updated
    expr: BExpr; //relation between new and old variables
    } with    
    override this.ToString() = 
            let var_str = Seq.map(fun v -> "p[k]." + v) this.var |> Seq.reduce(fun a b -> a + ", " + b) in                        
            var_str + "~=" + this.expr.ToString()
    member this.Simplfy() = RelationRule.Create(this.var, this.expr.Simplify())
    static member Create(var,expr) = {var=var; expr=expr}

type UpdateRule = 
    |Assignment of AssignmentRule
    |Relation of RelationRule
    override this.ToString() = 
        match this with
        |Assignment r -> r.ToString()
        |Relation r ->  r.ToString()        
    member this.ToLatex() = 
        match this with
        |Assignment r -> r.ToLatex()
        |Relation r ->  failwith "not implemented"
    static member Create(var,expr) = Assignment(AssignmentRule.Create(var, expr))
    static member CreateRelation(var,expr) = Relation(RelationRule.Create(var, expr))
    member this.Simplify() = 
        match this with
        |Assignment r -> Assignment(r.Simplfy())
        |Relation r ->  Relation(r.Simplfy())



type Update = {
    guard: Option<BExpr>;
    rules: seq<UpdateRule>;
    } with     
    override this.ToString() = 
        let guard = match this.guard with
                    |Some g -> "{" + g.ToString() + "} "
                    |None -> "" in
        "update " + guard + "\n" + (Seq.map(fun r -> "   " + r.ToString()) this.rules|>Seq.reduce(fun a b -> a + ",\n" + b)) + ";"
   
    member this.ToLatex() = 
        let updates = 
            if Seq.isEmpty this.rules then "(preserve)" else
                @"\left\{\begin{array}{c}" + (Seq.map(fun (r:UpdateRule) -> r.ToLatex()) this.rules        
                |>Seq.reduce(fun a b -> a + @"\\" + b)) + @"\end{array}\right\}"
        let guard = match this.guard with
                    |Some g -> @"\mbox{if }" + g.ToLatex() + "\mbox{ then }"
                    |None -> "" in
        guard + updates       
    member this.Simplify() =
        match this.guard with 
        |Some(g) -> Update.Create(Some(g.Simplify()),Seq.map(fun (r:UpdateRule) -> r.Simplify()) this.rules)
        |None -> Update.Create(None,Seq.map(fun (r:UpdateRule) -> r.Simplify()) this.rules)       
    static member Create(guard,rules) = {guard = guard; rules = rules}


//given a list of finite paths with max. timestep, translate the dynamics to constraints
let GenerateConstraints paths stateVarTypes (updates:seq<Update>) = 
    let stateVars = Seq.map(fun x -> fst x) (stateVarTypes |> Map.toSeq) |> Set.ofSeq in
    let init p k = LOr(Seq.map(fun (U:Update) ->          
                        let updateCst, updatedVars = 
                            Seq.fold(fun (uc,uv) (u:UpdateRule) -> 
                                            match u with 
                                            |Assignment a -> 
                                                 let e = a.expr in
                                                 let ie = e.Init p k in //instantiate the assignment expression to the path and timestep 

                                                 //generate the rules for the updated species
                                                 let updated = match ie with 
                                                               |NExpr(e) -> BTerm(BComp(Eq(NTerm(NVar(StateVar(p,k,a.var))),e)))
                                                               |BExpr(e) -> Beq(BTerm(BVar(StateVar(p,k,a.var))),e)                                 
                                                 in                         
                                                 
                                                 let updated_cst = Seq.append uc [updated] in //update constraints
                                                 let updated_vars = Seq.append uv [a.var] in  //modified variables
                                                                                                  
                                                 updated_cst, updated_vars

                                            |Relation r -> 
                                                 let e:BExpr = r.expr in
                                                 let updated_cst = Seq.append uc [e.Init p k] in     //update constraints
                                                 let updated_vars = Seq.append uv r.var in          //modified variables                                                 
                                                                                                                                                   
                                                 updated_cst, updated_vars
                                      ) (Seq.empty, Seq.empty) U.rules     
                                                   
                        let updateCst = LAnd(updateCst) in //construct an AND statement

                        //take each state variable name and find the updated varialbes                    
                        let preservedVars =  stateVars - Set.ofSeq(updatedVars) |> Set.toSeq in
                    
                        //encode the identity constraint for bool and num. vars that remain unchanged
                        let preserveVars = 
                            Seq.map(fun v -> 
                                match stateVarTypes.Item(v) with
                                | Bool -> Beq(BTerm(BVar(StateVar(p,k,v))),BTerm(BVar(StateVar(p,k-1,v))))
                                | _ -> BTerm(BComp(Eq(NTerm(NVar(StateVar(p,k,v))),NTerm(NVar(StateVar(p,k-1,v))))))
                                ) preservedVars in                                                                     
                        if Seq.isEmpty preserveVars then 
                            //include the guard if there is one                                                            
                            match U.guard with
                                |Some(g) -> And(g.Init p k,updateCst)
                                |None -> updateCst         
                        else                                              
                            let preserveCst = LAnd(preserveVars) in
                            match U.guard with
                                |Some(g) -> And(g.Init p k,And(updateCst,preserveCst))
                                |None -> And(updateCst,preserveCst)                             
                        ) updates)                    
    in  

    (*NOTE: dynamics are generated by applying the update rules starting at step 1..
    It might make sense to apply the rules at step 0 but this creates problems with
    "Garden-of-Eden" states, which do not have a valid predecessor. By default,
    non-deterministic choice variables are created for steps less than 0.
    *)    
    Seq.map(fun (p,_,max_k) -> Seq.map(fun k -> init p k) [1..max_k] ) paths
    |> Seq.fold(fun acc s -> Seq.append(acc) s) Seq.empty
    //|> Seq.map(fun x -> BExpr(x))     
