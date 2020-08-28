module Microsoft.Research.ReasoningEngine.Transformations

open Model
open Constraint
open Var
open Dynamics


let negate (model:Model) = 
   let nexp = Seq.map(fun (e, d) -> Not(e), "negation of " + d) model.constraints.observations in
   let ne = Seq.map fst nexp |> LOr
   {model with constraints={model.constraints with observations=[ne,"negation"]}}

//temp place for this function: move to solution.fs?
let solution (model:Model) (map:Map<Var,string>) = 
    let varTypes = model.system.varDefs |> Map.map(fun n v -> v.t) in
    let cst = Seq.map(fun ((var:Var), valstr) ->
                match varTypes.[var.Name] with
                |Int 
                |BInt _ 
                |Nat _
                |BNat _ -> 
                    let value = System.Int32.Parse(valstr) in 
                    BTerm(BComp(Eq(NTerm(NVar(var)),NTerm(NConst(value)))))
                |Bool -> 
                    let value = if valstr = "true" then true else false
                    Beq(BTerm(BVar(var)),BTerm(BConst(value)))
                ) (map |> Map.toSeq)    
    
    Seq.fold(fun (model:Model) c -> model.Assert(c)) model cst
    
let abstraction (model:Model) (predicate:BExpr) = 
    let updatedVars = model.system.stateVars |> Seq.map(fun v -> v.Key) in
    {model with system = {model.system with updates=[Update.Create(None,[UpdateRule.CreateRelation(updatedVars,predicate)])]}}