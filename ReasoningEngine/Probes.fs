module Microsoft.Research.ReasoningEngine.TProbes
open Model
open Var
open System.Collections.Generic

let ContainsUnbounded (S:DSystem) = 
    Seq.tryFind(fun (v:KeyValuePair<string,VarDef>) -> v.Value.t=Type.Int) S.varDefs

let ContainsBounded (S:DSystem) = 
    Seq.tryFind(fun (v:KeyValuePair<string,VarDef>) -> 
        match v.Value.t with 
        | Type.Nat _ -> true
        | Type.BNat _ -> true
        | Type.BInt _ -> true
        | _ -> false
        ) S.varDefs   

    
   

let IsNonMarkovian(S:DSystem) = 
    let isNonMarkovianRule (r:Dynamics.UpdateRule) = 
        let vars = match r with
                   |Dynamics.Assignment(e) ->  e.expr.Vars()
                   |Dynamics.Relation(e) -> e.expr.Vars()
        in
        Seq.exists(fun (v:Var) -> 
                    match v.Delay with 
                    | Some(n) when n <> -1 -> true 
                    | _ -> false                
              ) vars
    in
    Seq.exists(fun (u:Dynamics.Update) -> 
        Seq.exists(fun r -> isNonMarkovianRule r) u.rules) S.updates