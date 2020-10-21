module Microsoft.Research.ReasoningEngine.Encodings

open System
open Microsoft.Z3
open System.Collections.Generic
open Var
open Constraint
open Settings
open Model
open System.ComponentModel
open System.IO


type Z3Encoding<'a> = 
    { EncodeBExpr   : BExpr -> BoolExpr
    ; EncodeCExpr   : CExpr -> BoolExpr
    ; EncodeNExpr   : NExpr -> 'a    
    ; Vars          : Map<Var,Microsoft.Z3.Expr>
    ; ctx           : Context
    ; cst           : List<BoolExpr>
    } with 
       
    static member MkBoolIntEncoding (z3:Context) (V:seq<Var>) (Vd:Map<string,VarDef>) (P:Map<string,BExpr>)=         
        let IntSort = z3.MkIntSort() in        
        let BoolSort = z3.MkBoolSort() in

        let vars = 
                seq {for v in V do 
                        match v with 
                        | ConcVar(v) -> 
                            let name = v.ToString() in                           
                            let t = Vd.[v.Name].t in
                            let encoding = if t=Var.Bool then 
                                            z3.MkFreshConst(name,BoolSort)
                                           else
                                            z3.MkFreshConst(name,IntSort) in
                            yield (v,encoding)
                        | _ -> ()
                    } |> Seq.cache |> Map.ofSeq in

        let zero = z3.MkInt(0):>ArithExpr in    
        let one  = z3.MkInt(1):>ArithExpr in    
        let rec EncodeNTerm (t:NTerm) = 
            match t with       
            | NConst(n) -> z3.MkInt(n):>ArithExpr        
            | Ite(c,e1,e2) -> z3.MkITE(EncodeBExpr c, EncodeNExpr e1, EncodeNExpr e2):?>ArithExpr    
            | NVar(v) -> Lookup v :?> ArithExpr //given that this variable is used in an arithmetic expression, its type should be ArithExpr
            | Card(vs) -> if Seq.isEmpty vs then zero else
                          Seq.map(fun v -> z3.MkITE(EncodeBExpr v, one, zero):?>ArithExpr) vs
                          |> Seq.reduce(fun a b -> z3.MkAdd(a,b))                                            
        and EncodeNExpr (e:NExpr)  =
            match e with       
            | NTerm(t) -> EncodeNTerm(t)
            | Neg(e) -> z3.MkSub(zero,EncodeNExpr e)
            | Add(e1,e2) -> z3.MkAdd(EncodeNExpr e1, EncodeNExpr e2)
            | Sub(e1,e2) -> z3.MkSub(EncodeNExpr e1, EncodeNExpr e2)
            | Mul(e1,e2) -> z3.MkMul(EncodeNExpr e1, EncodeNExpr e2)                          
        and EncodeCExpr (e:CExpr)  =
            match e with
            | Gt(e1,e2) -> z3.MkGt(EncodeNExpr e1, EncodeNExpr e2)
            | Ge(e1,e2) -> z3.MkGe(EncodeNExpr e1, EncodeNExpr e2)
            | Lt(e1,e2) -> z3.MkLt(EncodeNExpr e1, EncodeNExpr e2)
            | Le(e1,e2) -> z3.MkLe(EncodeNExpr e1, EncodeNExpr e2)
            | Eq(e1,e2) -> z3.MkEq(EncodeNExpr e1, EncodeNExpr e2)            
        and EncodePredicate(pred:Pred) = 
            match pred with
            | ConcPred(n,p,k) -> EncodeBExpr (P.[n].Init p k)
            | _ -> failwith "Abstract predicates cannot be encoded yet"
        and EncodeBTerm(t:BTerm) = 
            match t with
            | BConst(true) -> z3.MkTrue()
            | BConst(false) -> z3.MkFalse()
            | BComp(e) -> EncodeCExpr e
            | BVar(v) -> Lookup v :?> BoolExpr //given that this variable is used in a boolean expression, its type should be BoolExpr
            | Predicate(pred) -> EncodePredicate pred //inline predicate
            | Fixpoint(p,k) -> EncodeBExpr (P.["$Terminal"].Init p k)
        and EncodeBExpr (e:BExpr)  =
            match e with
            | BTerm(t) -> EncodeBTerm t                        
            | Beq(e1,e2) -> z3.MkEq(EncodeBExpr e1, EncodeBExpr e2)
            | Imp(e1,e2) -> z3.MkImplies(EncodeBExpr e1, EncodeBExpr e2)
            | And(e1,e2) -> z3.MkAnd(EncodeBExpr e1, EncodeBExpr e2)
            | Or(e1,e2) -> z3.MkOr(EncodeBExpr e1, EncodeBExpr e2)
            | LAnd(el) -> z3.MkAnd(Seq.map(fun (e:BExpr) -> EncodeBExpr e) el |> Seq.toArray)
            | LOr(el) -> z3.MkOr(Seq.map(fun (e:BExpr) -> EncodeBExpr e) el |> Seq.toArray)
            | Not(e1) -> z3.MkNot(EncodeBExpr e1)              
        and Lookup (v:Var) :Microsoft.Z3.Expr =
            match v with 
            | ConcVar(v) -> vars.[v]
            | _ -> raise (new System.NotSupportedException("Abstract variable encoding"))            
        //return the encoding    
        { EncodeBExpr = EncodeBExpr
        ; EncodeCExpr = EncodeCExpr
        ; EncodeNExpr = EncodeNExpr        
        ; Vars = vars
        ; ctx = z3
        ; cst = new List<BoolExpr>()}    

    //BY: currently, all variables are encoded as a fixed-width bit-vector.
    //TODO: optimize the encoding with variable-size bit-vectors?
    //TODO: apply a check that all variables are bounded and fail otherwise?    
    static member MkBVEncoding (size:uint32) (z3:Context) (V:seq<Var>) (Vd:Map<string,VarDef>) (P:Map<string,BExpr>) =       
       

        let bvsize n = if n = 0 then (uint32) 1 else (uint32) (Math.Floor(Math.Log((float)n,2.0) + 1.0))
        let fixBVsize (b:BitVecExpr) (b':BitVecExpr) ext =                                 
                let size' = 
                    if b.SortSize > b'.SortSize then b.SortSize + (uint32) ext
                    else b'.SortSize + (uint32) ext
                let b = z3.MkZeroExt(size' - b.SortSize,b)
                let b' = z3.MkZeroExt(size' - b'.SortSize,b')      
                (b,b')


        let BV = z3.MkBitVecSort(size)
        let vars = 
                seq {for v in V do 
                        match v with 
                        | ConcVar(v) -> 
                            match Vd.[v.Name].t with                            
                            | Bool -> yield (v,z3.MkFreshConst(v.ToString(),z3.BoolSort))
                            | _ -> yield (v,z3.MkFreshConst(v.ToString(),BV)) //TODO: add error checking for various parameter types (maybe check the bounds to constraints tactic)                                                        
                        | _ -> ()
                    } |> Seq.cache |> Map.ofSeq in
        let rec EncodeNTerm (t:NTerm) = 
            match t with       
            | NConst(n) -> z3.MkBV(n,bvsize n) :> BitVecExpr
            | Ite(c,e1,e2) -> z3.MkITE(EncodeBExpr c, EncodeNExpr e1, EncodeNExpr e2):?>BitVecExpr
            | NVar(v) -> Lookup v :?> BitVecExpr
            | Card(vs) -> failwith "Cardinality constraint is not yet implemented for bit-vector encoding"                                         
        and EncodeNExpr (e:NExpr)  = 
            match e with       
            | NTerm(t) -> EncodeNTerm(t)
            | Neg(e) -> failwith "Negative numbers are not yet implemented for bit-vector encoding"
            | Add(e1,e2) -> 
                let e1', e2' = fixBVsize (EncodeNExpr e1) (EncodeNExpr e2) 1
                z3.MkBVAdd(e1', e2')
            | Sub(e1,e2) -> 
                let e1', e2' = fixBVsize (EncodeNExpr e1) (EncodeNExpr e2) 0    //this is not safe in terms of underflows
                z3.MkBVSub(e1', e2')
            | Mul(e1,e2) -> 
                let e1', e2' = fixBVsize (EncodeNExpr e1) (EncodeNExpr e2) 0  //this should be the (size of a) + (size of b)?
                z3.MkBVMul(e1', e2')
        and EncodeCExpr (e:CExpr)  =
            match e with
            | Gt(e1,e2) -> 
                let e1', e2' = fixBVsize (EncodeNExpr e1) (EncodeNExpr e2) 0                             
                z3.MkBVUGT(e1', e2')
            | Ge(e1,e2) -> 
                let e1', e2' = fixBVsize (EncodeNExpr e1) (EncodeNExpr e2) 0                              
                z3.MkBVUGE(e1', e2')
            | Lt(e1,e2) -> 
                let e1', e2' = fixBVsize (EncodeNExpr e1) (EncodeNExpr e2) 0                              
                z3.MkBVULT(e1', e2')
            | Le(e1,e2) -> 
                let e1', e2' = fixBVsize (EncodeNExpr e1) (EncodeNExpr e2) 0                               
                z3.MkBVULE(e1', e2')
            | Eq(e1,e2) -> 
                let e1', e2' = fixBVsize (EncodeNExpr e1) (EncodeNExpr e2) 0                              
                z3.MkEq(e1', e2')
        and EncodePredicate(pred:Pred) = 
            match pred with
            | ConcPred(n,p,k) -> EncodeBExpr (P.[n].Init p k)
            | _ -> failwith "Abstract predicates cannot be encoded yet"
        and EncodeBTerm(t:BTerm) = 
            match t with
            | BConst(true) -> z3.MkTrue()
            | BConst(false) -> z3.MkFalse()
            | BComp(e) -> EncodeCExpr e
            | BVar(v) -> Lookup v :?> BoolExpr
            | Predicate(pred) -> EncodePredicate pred  //inline predicate
            | Fixpoint(p,k) -> EncodeBExpr (P.["$Terminal"].Init p k)
        and EncodeBExpr (e:BExpr)  =
            match e with
            | BTerm(t) -> EncodeBTerm t                        
            | Beq(e1,e2) -> z3.MkEq(EncodeBExpr e1, EncodeBExpr e2)
            | Imp(e1,e2) -> z3.MkImplies(EncodeBExpr e1, EncodeBExpr e2)
            | And(e1,e2) -> z3.MkAnd(EncodeBExpr e1, EncodeBExpr e2)
            | Or(e1,e2) -> z3.MkOr(EncodeBExpr e1, EncodeBExpr e2)
            | LAnd(el) -> z3.MkAnd(Seq.map(fun (e:BExpr) -> EncodeBExpr e) el |> Seq.toArray)
            | LOr(el) -> z3.MkOr(Seq.map(fun (e:BExpr) -> EncodeBExpr e) el |> Seq.toArray)
            | Not(e1) -> z3.MkNot(EncodeBExpr e1)              
        and Lookup (v:Var) :Microsoft.Z3.Expr =
            match v with 
            | ConcVar(v) -> vars.[v]
            | _ -> raise (new System.NotSupportedException("Abstract variable encoding"))            
        //return the encoding    
        { EncodeBExpr = EncodeBExpr
        ; EncodeCExpr = EncodeCExpr
        ; EncodeNExpr = EncodeNExpr        
        ; Vars = vars
        ; ctx = z3
        ; cst =  new List<BoolExpr>()}    

    member this.Encode (model:Model) = 
        //apply the encoding to all constraints
        let cst = 
            model.constraints.GetConstraints() 
            |> Seq.map(fun x -> this.EncodeBExpr x)        
            |> Seq.append this.cst
            |> Seq.toArray
            |> fun e -> this.ctx.MkAnd(e)            
        //return the encoded constraints and variables
        (cst,this.Vars)

    member this.EncodeCore (model:Model) = 
        //apply the encoding to all constraints, introducing a choice variable for each expression
        let choice, cst = 
            model.constraints.GetConstraints()
            |> Seq.toList //note that caching or converting the sequence to list here is critical (otherwise, fresh variables are re-defined later)
            |> List.map(fun e -> 
                let b = this.ctx.MkFreshConst("b",this.ctx.BoolSort) :?>BoolExpr
                b, this.ctx.MkImplies(b,this.EncodeBExpr e))
            |> List.unzip  
    
        //construct a conjunction of all constraints
        let cst = 
            cst
            |> Seq.append this.cst
            |> Array.ofSeq
            |> fun e -> this.ctx.MkAnd(e)       

        //return the result          
        (choice, cst, this.Vars)    
    

    static member MkEncoding encFn (model:Model) =      
        Microsoft.Z3.Global.SetParameter("model.compact","false") //TODO: without this setting we get wrong models with Z3 4.8.1

        let z3 = new Context() //construct the context                      
        encFn z3 (model.Vars()) model.system.varDefs model.constraints.predicates //construct the encoding and return the encoded model 

    static member Encode (model:Model) =         
        Microsoft.Z3.Global.SetParameter("model.compact","false") //TODO: without this setting we get wrong models with Z3 4.8.1
        let z3 = new Context() //construct the context            
        //construct the encoding and return the encoded model 
        match model.settings.encoding with
        | Settings.Integer -> 
            let encoding = Z3Encoding<_>.MkBoolIntEncoding z3 (model.Vars()) model.system.varDefs model.constraints.predicates 
            encoding.ctx, encoding.Encode model
        | Settings.BitVector(size) ->
            let encoding = Z3Encoding<_>.MkBVEncoding size z3 (model.Vars()) model.system.varDefs model.constraints.predicates 
            encoding.ctx, encoding.Encode model
                        
 

//encoding a sequence of RE expressions as Z3 expressions using the integer theory with Booleans
let ToZ3BoolInt (z3:Context) (model:Model) =    
    let encoding = Z3Encoding<_>.MkBoolIntEncoding z3 (model.Vars()) model.system.varDefs model.constraints.predicates    //construct an encoding
    encoding.Encode model //return the encoded constraints and variables
    
//encoding a sequence of RE expressions as Z3 expressions using the integer theory with Booleans
let ToZ3BoolIntUNSATCORE (z3:Context) (model:Model) =         
    let encoding = Z3Encoding<_>.MkBoolIntEncoding z3 (model.Vars()) model.system.varDefs model.constraints.predicates     //construct an encoding
    encoding.EncodeCore model    //return the encoded constraints and variables

let ToZ3BV (z3:Context) (model:Model) size =   
    let encoding = Z3Encoding<_>.MkBVEncoding size z3 (model.Vars()) model.system.varDefs model.constraints.predicates  //construct an encoding    
    encoding.Encode model //return the encoded constraints and variables




//BY: EXPERIMENTAL FUNCTIONALITY

type InferVarBase = 
    | IBool of Microsoft.ML.Probabilistic.Models.Variable<bool>*string
    | IDisc of Microsoft.ML.Probabilistic.Models.Variable<int>*string

type InferVarProvider =     
    { mutable hash : Map<string,InferVarBase>
    }    

    static member Init() = {hash = Map.empty}

    member this.MkHashedVar (v:InferVarBase) = 
        let id = 
            match v with
            | IBool (v,_) -> sprintf "BOOL_%i" (v.GetHashCode())//vv
            | IDisc (v,_) -> sprintf "NUM_%i" (v.GetHashCode())//vv
        
        if this.hash.ContainsKey id then 
            this.hash.[id]
        else            
            this.hash <- this.hash.Add(id, v)
            v
            
    member this.MkNumVar id (v:Microsoft.ML.Probabilistic.Models.Variable<int>)  = IDisc (v,id) |>  this.MkHashedVar
    
    member this.MkBoolVar id (v:Microsoft.ML.Probabilistic.Models.Variable<bool>) = IBool (v,id) |>  this.MkHashedVar
        
    static member GetBoolVar (v:InferVarBase) = match v with IBool (vv,_) -> vv | IDisc (_,id) -> failwithf "Not Boolean: %s" id
    
    static member GetNumVar (v:InferVarBase) = match v with IDisc (vv,_) -> vv | IBool (_,id) -> failwithf "Not Numerical: %s" id

    static member GetID (v:InferVarBase) = match v with IDisc (_,id) -> id | IBool (_,id) -> id

    member this.GetBoolVar (v:InferVarBase) = InferVarProvider.GetBoolVar v
    
    member this.GetNumVar (v:InferVarBase) = InferVarProvider.GetNumVar v

    member this.GetID (v:InferVarBase) = InferVarProvider.GetID v

    
type InferEncoding = 
    { EncodeBExpr   : BExpr -> InferVarBase
    ; EncodeCExpr   : CExpr -> InferVarBase
    ; EncodeNExpr   : NExpr -> InferVarBase
    ; Vars          : Map<Var,InferVarBase>        
    } with 
    static member MkEncoding (InferVar:InferVarProvider) (V:seq<Var>) (Vd:Map<string,VarDef>) (P:Map<string,BExpr>)=                 

        let vars = 
            V 
            |> Seq.choose(fun v ->                 
                        match v with 
                        | ConcVar(v) ->                             
                            let t = Vd.[v.Name].t
                            let name = v.ToString()
                            let encoding = 
                                match t with
                                | Var.Bool   -> Microsoft.ML.Probabilistic.Models.Variable.Bernoulli(0.5)                                  |> InferVar.MkBoolVar name
                                | Var.BNat n -> Microsoft.ML.Probabilistic.Models.Variable.Discrete(Array.init n (fun _ -> 1.0/(float n))) |> InferVar.MkNumVar name
                                | _          -> failwith (sprintf "Variable type %A not supported yet" t)
                            Some (v,encoding)
                        | _ -> None
                    )            
            |> Map.ofSeq


        //let mutable factors: Map<string, Microsoft.Msagl.Drawing.Node>     = Map.empty
        //let mutable edges : Map<(string*string),Microsoft.Msagl.Drawing.Edge                                

        let rec EncodeNTerm (t:NTerm) =             
            match t with       
            | NConst n  -> Microsoft.ML.Probabilistic.Models.Variable.Constant n |> InferVar.MkNumVar (sprintf "%i" n)
            | Ite _     -> failwith "ITE expressions not supported yet"
            | NVar v    -> Lookup v
            | Card _    -> failwith "Cardinality expressions not supported yet"
 
        and EncodeNExpr (e:NExpr)  =
            let v = 
                match e with       
                | NTerm t    -> t |> EncodeNTerm |> InferVar.GetNumVar
                | Neg e      -> Microsoft.ML.Probabilistic.Models.Variable.Constant(0) - (EncodeNExpr e |> InferVar.GetNumVar)
                | Add(e1,e2) -> (EncodeNExpr e1 |> InferVar.GetNumVar) + (EncodeNExpr e2 |> InferVar.GetNumVar)
                | Sub(e1,e2) -> (EncodeNExpr e1 |> InferVar.GetNumVar) - (EncodeNExpr e2 |> InferVar.GetNumVar)
                | Mul(e1,e2) -> (EncodeNExpr e1 |> InferVar.GetNumVar) * (EncodeNExpr e2 |> InferVar.GetNumVar)        
            
            let id = ""
                //match e with       
                //| NTerm t    -> t |> EncodeNTerm |> InferVar.GetID
                //| Neg e      -> sprintf "NEG(%s)" (EncodeNExpr e |> InferVar.GetID)
                //| Add(e1,e2) -> sprintf "ADD(%s,%s)" (EncodeNExpr e1 |> InferVar.GetID) (EncodeNExpr e2 |> InferVar.GetID)
                //| Sub(e1,e2) -> sprintf "SUB(%s,%s)" (EncodeNExpr e1 |> InferVar.GetID) (EncodeNExpr e2 |> InferVar.GetID)
                //| Mul(e1,e2) -> sprintf "MUL(%s,%s)" (EncodeNExpr e1 |> InferVar.GetID) (EncodeNExpr e2 |> InferVar.GetID)        

            v |> InferVar.MkNumVar id

        and EncodeCExpr (e:CExpr)  =
            let v = 
                match e with
                | Gt(e1,e2) -> Microsoft.ML.Probabilistic.Models.Variable<int>.op_GreaterThan       (EncodeNExpr e1 |> InferVar.GetNumVar, EncodeNExpr e2 |> InferVar.GetNumVar)
                | Ge(e1,e2) -> Microsoft.ML.Probabilistic.Models.Variable<int>.op_GreaterThanOrEqual(EncodeNExpr e1 |> InferVar.GetNumVar, EncodeNExpr e2 |> InferVar.GetNumVar)
                | Lt(e1,e2) -> Microsoft.ML.Probabilistic.Models.Variable<int>.op_LessThan          (EncodeNExpr e1 |> InferVar.GetNumVar, EncodeNExpr e2 |> InferVar.GetNumVar)
                | Le(e1,e2) -> Microsoft.ML.Probabilistic.Models.Variable<int>.op_LessThanOrEqual   (EncodeNExpr e1 |> InferVar.GetNumVar, EncodeNExpr e2 |> InferVar.GetNumVar)
                | Eq(e1,e2) -> Microsoft.ML.Probabilistic.Models.Variable<int>.op_Equality          (EncodeNExpr e1 |> InferVar.GetNumVar, EncodeNExpr e2 |> InferVar.GetNumVar)   

            let id = ""
                //match e with
                //| Gt(e1,e2) -> sprintf "Gt(%s,%s)" (EncodeNExpr e1 |> InferVar.GetID) (EncodeNExpr e2 |> InferVar.GetID)
                //| Ge(e1,e2) -> sprintf "Ge(%s,%s)" (EncodeNExpr e1 |> InferVar.GetID) (EncodeNExpr e2 |> InferVar.GetID)
                //| Lt(e1,e2) -> sprintf "Lt(%s,%s)" (EncodeNExpr e1 |> InferVar.GetID) (EncodeNExpr e2 |> InferVar.GetID)
                //| Le(e1,e2) -> sprintf "Le(%s,%s)" (EncodeNExpr e1 |> InferVar.GetID) (EncodeNExpr e2 |> InferVar.GetID)
                //| Eq(e1,e2) -> sprintf "Eq(%s,%s)" (EncodeNExpr e1 |> InferVar.GetID) (EncodeNExpr e2 |> InferVar.GetID)   

            v |> InferVar.MkBoolVar id

        and EncodePredicate(pred:Pred) = 
            match pred with
            | ConcPred(n,p,k) -> EncodeBExpr (P.[n].Init p k)
            | _ -> failwith "Abstract predicates cannot be encoded yet"

        and EncodeBTerm(t:BTerm) =             
            match t with
            | BConst true    -> Microsoft.ML.Probabilistic.Models.Variable.Constant(true) |> InferVar.MkBoolVar "true"
            | BConst false   -> Microsoft.ML.Probabilistic.Models.Variable.Constant(false) |> InferVar.MkBoolVar "false"
            | BComp e        -> EncodeCExpr e
            | BVar v         -> Lookup v 
            | Predicate pred -> EncodePredicate pred //inline predicate
            | Fixpoint(p,k)  -> EncodeBExpr (P.["$Terminal"].Init p k)            

        and EncodeBExpr (e:BExpr)  =
            let v = 
                match e with
                | BTerm t    -> EncodeBTerm t |> InferVar.GetBoolVar
                | Beq(e1,e2) -> Microsoft.ML.Probabilistic.Models.Variable<bool>.op_Equality(EncodeBExpr e1 |> InferVar.GetBoolVar,  EncodeBExpr e2 |> InferVar.GetBoolVar)
                | Imp(e1,e2) ->  //translate e1 -> e2 to !e1 \/ e2 and encode
                    let E1 = EncodeBExpr e1 |> InferVar.GetBoolVar
                    let E2 = EncodeBExpr e2 |> InferVar.GetBoolVar
                    Microsoft.ML.Probabilistic.Models.Variable<bool>.op_BitwiseOr(Microsoft.ML.Probabilistic.Models.Variable<bool>.op_LogicalNot(E1), E2)                                
                | And(e1,e2) -> Microsoft.ML.Probabilistic.Models.Variable<bool>.op_BitwiseAnd(EncodeBExpr e1 |> InferVar.GetBoolVar, EncodeBExpr e2 |> InferVar.GetBoolVar)
                | Or(e1,e2) ->  Microsoft.ML.Probabilistic.Models.Variable<bool>.op_BitwiseOr(EncodeBExpr e1 |> InferVar.GetBoolVar, EncodeBExpr e2 |> InferVar.GetBoolVar)
                | LAnd(el) -> el |> Seq.map (EncodeBExpr >> InferVar.GetBoolVar) |> Seq.reduce(fun a b -> Microsoft.ML.Probabilistic.Models.Variable<bool>.op_BitwiseAnd(a,b))
                | LOr(el) -> el |> Seq.map (EncodeBExpr  >> InferVar.GetBoolVar) |> Seq.reduce(fun a b -> Microsoft.ML.Probabilistic.Models.Variable<bool>.op_BitwiseOr(a,b))
                | Not(e1) -> Microsoft.ML.Probabilistic.Models.Variable<bool>.op_LogicalNot(EncodeBExpr e1 |> InferVar.GetBoolVar)              

            let id = ""
                //match e with
                //| BTerm(t)   -> EncodeBTerm t |> InferVar.GetID
                //| Beq(e1,e2) -> sprintf "BEQ(%s,%s)" (EncodeBExpr e1 |> InferVar.GetID) (EncodeBExpr e2 |> InferVar.GetID)
                //| Imp(e1,e2) -> sprintf "IMP(%s,%s)" (EncodeBExpr e1 |> InferVar.GetID) (EncodeBExpr e2 |> InferVar.GetID)                    
                //| And(e1,e2) -> sprintf "AND(%s,%s)" (EncodeBExpr e1 |> InferVar.GetID) (EncodeBExpr e2 |> InferVar.GetID)
                //| Or(e1,e2)  -> sprintf "OR(%s,%s)"  (EncodeBExpr e1 |> InferVar.GetID) (EncodeBExpr e2 |> InferVar.GetID)
                //| LAnd el    -> el |> Seq.map (EncodeBExpr >> InferVar.GetID) |> String.concat "," |> sprintf "AND(%s)"
                //| LOr el     -> el |> Seq.map (EncodeBExpr  >> InferVar.GetID) |> String.concat "," |> sprintf "OR(%s)"
                //| Not e1     -> EncodeBExpr e1 |> InferVar.GetID |> sprintf "NOT(%s)"

            v |> InferVar.MkBoolVar id

        and Lookup (v:Var) =
            match v with 
            | ConcVar(v) -> vars.[v]
            | _ -> raise (new System.NotSupportedException("Abstract variable encoding"))            

        { EncodeBExpr = EncodeBExpr
        ; EncodeCExpr = EncodeCExpr
        ; EncodeNExpr = EncodeNExpr        
        ; Vars    = vars        
        }    
          
    static member Encode (model:Model) =         
        let InferVar = InferVarProvider.Init() 

        //construct the encoding and return the encoded model         
        let encoding = InferEncoding.MkEncoding InferVar (model.Vars()) model.system.varDefs model.constraints.predicates 

        model.constraints.GetConstraints() 
            |> Seq.map(fun x -> encoding.EncodeBExpr x)                    
            |> Seq.toArray
            |> Seq.iter (InferVarProvider.GetBoolVar >> Microsoft.ML.Probabilistic.Models.Variable.ConstrainTrue)//e.ObservedValue <- true)

        System.Console.Error.WriteLine("Finished encoding to Infer.NET ({0} total factors)", InferVar.hash.Count)
        encoding.Vars
        