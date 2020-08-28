module Microsoft.Research.ReasoningEngine.Checks
open Model
open Dynamics
open Var
open Constraint

//ensure that the variables shared between two models (based on name) have the same scope and types
let SharedVarCheck (M1:DSystem) (M2:DSystem) = 
    let CheckVar (v:VarDef)  = 
        if M2.varDefs.ContainsKey(v.name) then //M2 contains a variable with the same name
            let v2 = M2.varDefs.Item(v.name) in            
            (v.scope = v2.scope) && (v.t=v2.t) //the check passes if the scope and types match
        else 
            true //the check passes: M2 does not contain a variable with the same name
    in        

    let vars = M1.varDefs |> Map.toList |> Seq.map(fun x -> snd x) in //take all variables of the first model
    match (Seq.tryFind(fun v -> not (CheckVar v)) vars) with //check if either variable fails the check
    |Some _ -> false //some problems with types or scope were discovered (optionally, return the problematic values?)
    |None -> true    //the check passed: all variable scopes and types are consistent


//Currently, typecheck only update rules.
//a numerical (boolean) variable must be updated with a numerical (boolean) expression only
let TypeCheck (model:Model) =     
    let UpdateRuleChecker (rule:UpdateRule) =         
        match rule with 
        |Assignment r -> 
            let var = model.system.varDefs.[r.var] in        
            match var.t,r.expr with
            |Bool,NExpr _ -> failwith "Numerical update rule was applied to a Boolean variable"
            |Nat,BExpr _ -> failwith "Logical update rule  was applied to a numerical variable"
            |BNat _ ,BExpr _ -> failwith "Logical update rule  was applied to a numerical variable"
            |Int ,BExpr _ -> failwith "Logical update rule  was applied to a numerical variable"
            |BInt _ ,BExpr _ -> failwith "Logical update rule  was applied to a numerical variable"
            | _ -> ()                       
        |Relation r -> () //TODO: implement type checking for relation rules
    in
    let UpdateChecker (u:Update) = Seq.iter UpdateRuleChecker u.rules in

    //typecheck the update rules for consistent applications
    Seq.iter UpdateChecker model.system.updates

    model