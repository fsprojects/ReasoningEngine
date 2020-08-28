module Microsoft.Research.ReasoningEngine.CRN
//module for dealing with chemical reaction networks (CRN)
open Model
open Constraint
open Var
open System.Text.RegularExpressions
open System

type multiset = (string * int) list
type reaction = {products: multiset; reactants: multiset; catalysts:multiset}  
type CRN(S:string list,R:reaction list)  = 
    member this.species with get() = S;
    member this.reactions with get() = R;
    member this.encode = 
       let varDefs =  Seq.map(fun s -> s,VarDef(State,Nat,s)) this.species |> Map.ofSeq in
       let updates = Seq.map(fun r -> 
                        let guard = Seq.map(fun (s,n) -> BTerm(Ge(NVar(AbsStateVar(0,s)),NConst(n)))) (Seq.append(r.reactants) r.catalysts) 
                                    |> Seq.reduce(fun a b -> And(a,b)) in            
                        let prod_updates = Seq.map(fun (s,n) -> Dynamics.UpdateRule(s,NExpr(Sub(NVar(AbsStateVar(-1,s)),NConst(n))))) r.reactants in
                        let react_updates = Seq.map(fun (s,n) -> Dynamics.UpdateRule(s,NExpr(Add(NVar(AbsStateVar(-1,s)),NConst(n))))) r.products in
                        Dynamics.Update(Some(guard),(Seq.append(prod_updates) react_updates))) this.reactions in
       Model(varDefs, updates)


//very prelimianary parser for CRNs
let Parse(modelStr:string) =    
    let lines = modelStr.Split('\n') in
    let model = Seq.map(fun (x:string) -> let x = x.Replace("|","") in 
                                          let x = Regex.Replace(x,"\s(\d+)([^\s+])"," $1 $2") in 
                                          let x = Regex.Replace(x,"\>(\d+)([^\s+])","$1 $2") in
                                          let x = Regex.Replace(x,"^(\d+)([^\s+])","$1 $2") in
                                          x) lines in
    let parseSpecies (s:string) = 
        s.Split('+') 
        |> Seq.map(fun x ->
            let sp =  x.Trim() in
            let (n,sp) = if sp.Contains(" ") then let sp = sp.Split(' ') in (System.Int32.Parse(sp.[0]),sp.[1]) else (1,sp) in
            (sp,n)
        ) 
        |> List.ofSeq
    in
    let emp:(string * int )list = List.empty in
    let parseRxn (s:string) = 
        let (R,P,r) = if s.Contains("<->") then 
                        let rxn = s.Split([|"<->"|],System.StringSplitOptions.None) in 
                        (rxn.[0],rxn.[1],true) 
                        else
                            let rxn = s.Split([|"->"|],System.StringSplitOptions.None) in 
                            (rxn.[0],rxn.[1],false) in 
         if r then 
            [{products=parseSpecies P; reactants=parseSpecies R; catalysts=emp};
             {products=parseSpecies R; reactants=parseSpecies P; catalysts=emp}]
         else [{products=parseSpecies R; reactants = parseSpecies P; catalysts = emp}]           
    in

    let rxn = Seq.fold(fun acc x -> List.concat([acc; parseRxn(x)])) [] model in
    let species = Seq.fold(fun acc r-> let getSpecies r = if List.isEmpty r then List.empty else List.map(fun (s,n) -> s) r in
                                             List.concat([acc; getSpecies r.products; getSpecies r.reactants; getSpecies r.catalysts])) [] rxn
                  |> Seq.distinct 
                  |> List.ofSeq
    in     
    CRN(species,rxn)