module Microsoft.Research.ReasoningEngine.Constraint
open Var



//
//type 't BExprT =         
//    | BTerm of 't
//    | Beq of 't BExprT * 't BExprT //logical equivalence
//    | Imp of 't BExprT * 't BExprT //Implies
//    | And of 't BExprT * 't BExprT //logical AND
//    | Or  of 't BExprT * 't BExprT //logical OR
//    | LAnd of seq<'t BExprT>      //logical AND
//    | LOr  of seq<'t BExprT>      //logical OR
//    | Not of 't BExprT            //negation    
//    override this.ToString() = 
//        match this with
//        | BTerm(e) -> e.ToString()
//        | Beq(e1, e2) -> e1.ToString() + " <=> " + e2.ToString()
//        | Imp(e1, e2) -> e1.ToString() + " => " + e2.ToString()
//        | And(e1, e2) -> e1.ToString() + " && " + e2.ToString()
//        | Or(e1, e2) -> e1.ToString() + " || " + e2.ToString()
//        | LAnd(es) -> "AND(" + Lib.seq2str es + ")"
//        | LOr(es) -> "OR(" + Lib.seq2str es + ")"
//        | Not(e) -> "!" + e.ToString()
//    member this.map f = 
//        match this with
//        | BTerm(e) -> BTerm(f e)
//        | Beq(e1, e2) -> Beq(e1.map f, e2.map f)
//        | Imp(e1, e2) -> Imp(e1.map f, e2.map f)
//        | And(e1, e2) -> And(e1.map f, e2.map f)
//        | Or(e1, e2) -> Or(e1.map f, e2.map f)
//        | LAnd(es) -> LAnd(Seq.map (fun (e: 't BExprT) -> e.map f) es)
//        | LOr(es) -> LOr(Seq.map (fun (e: 't BExprT) -> e.map f) es)
//        | Not(e) -> Not(e.map f)
//    member this.fold f = 
//        match this with
//        | BTerm(e) -> f e
//        | Beq(e1, e2) -> Seq.append(e1.fold f)  (e2.fold f)
//        | Imp(e1, e2) -> Seq.append(e1.fold f)  (e2.fold f)
//        | And(e1, e2) -> Seq.append(e1.fold f)  (e2.fold f)
//        | Or(e1, e2) -> Seq.append(e1.fold f)  (e2.fold f)
//        | LAnd(es) -> Seq.collect(fun (e: 't BExprT) -> e.fold f) es                
//        | LOr(es) -> Seq.collect(fun (e: 't BExprT) -> e.fold f) es
//        | Not(e) -> e.fold f
//
//type 't CExprT =     
//    | Gt  of 't * 't  //greater than 
//    | Ge  of 't * 't  //greater than or equal
//    | Lt  of 't * 't  //less than
//    | Le  of 't * 't  //less than or equal
//    | Eq  of 't * 't  //numerical equal             
//    override this.ToString() = 
//        match this with
//        | Gt(e1,e2) -> e1.ToString() + " > " + e2.ToString()
//        | Ge(e1,e2) -> e1.ToString() + " >= " + e2.ToString()
//        | Lt(e1,e2) -> e1.ToString() + " < " + e2.ToString()
//        | Le(e1,e2) -> e1.ToString() + " <= " + e2.ToString()
//        | Eq(e1,e2) -> e1.ToString() + " = " + e2.ToString()
//    member this.map f = 
//        match this with
//        | Gt(e1,e2) -> Gt(f e1, f e2)
//        | Ge(e1,e2) -> Ge(f e1, f e2)
//        | Lt(e1,e2) -> Lt(f e1, f e2)
//        | Le(e1,e2) -> Le(f e1, f e2)
//        | Eq(e1,e2) -> Eq(f e1, f e2)
//    member this.fold f = 
//        match this with
//        | Gt(e1,e2) -> Seq.append(f e1) (f e2)
//        | Ge(e1,e2) -> Seq.append(f e1) (f e2)
//        | Lt(e1,e2) -> Seq.append(f e1) (f e2)
//        | Le(e1,e2) -> Seq.append(f e1) (f e2)
//        | Eq(e1,e2) -> Seq.append(f e1) (f e2)
//
//
//type 't NExprT =          
//    | NTerm of 't 
//    | Neg of 't NExprT
//    | Add of 't NExprT * 't NExprT
//    | Sub of 't NExprT * 't NExprT
//    | Mul of 't NExprT * 't NExprT
//    | Sum of seq<'t NExprT>        
//    override this.ToString() = 
//        match this with        
//        | NTerm(t) -> t.ToString()
//        | Neg(e) -> "-" + e.ToString()
//        | Add(e1, e2) -> e1.ToString() + " + " + e2.ToString()
//        | Sub(e1, e2) -> e1.ToString() + " - " + e2.ToString()
//        | Mul(e1, e2) -> e1.ToString() + " * " + e2.ToString()
//        | Sum(es) -> "SUM(" + Lib.seq2str es + ")"    
//    member this.map f = 
//        match this with        
//        | NTerm(t) -> NTerm(f t)
//        | Neg(e) -> Neg(e.map f)
//        | Add(e1, e2) -> Add(e1.map f, e2.map f)
//        | Sub(e1, e2) -> Sub(e1.map f, e2.map f)
//        | Mul(e1, e2) -> Mul(e1.map f, e2.map f)
//        | Sum(es) -> Sum(Seq.map(fun (e:'t NExprT) -> e.map f) es)
//    member this.fold f = 
//        match this with        
//        | NTerm(t) -> f t
//        | Neg(e) -> e.fold f
//        | Add(e1, e2) -> Seq.append(e1.fold f) (e2.fold f)
//        | Sub(e1, e2) -> Seq.append(e1.fold f) (e2.fold f)
//        | Mul(e1, e2) -> Seq.append(e1.fold f) (e2.fold f)
//        | Sum(es) -> Seq.collect(fun (e:'t NExprT) -> e.fold f) es
//
//
//
//type BTerm = 
//    | BConst of bool 
//    | BVar of Var
//    | VarComp of NVarExpr CExprT
//    | StateComp of NStateExpr CExprT
//    | Predicate of string * string * int //predicate term
//and NVarTerm = 
//    | NConst of int 
//    | NVar of Var
//and NStateTerm = string * int //path * step   
//and NVarExpr = NVarTerm NExprT
//and NStateExpr = NStateTerm NExprT
//
//
//and BExpr = {
//    expr: BTerm BExprT
//    } with
//    override this.ToString() = ""


//and Expr = 
//    |BExpr of BExpr
//    |





//    //| Ite of BExpr * 't NExpr * 't NExpr
//    // | Card of seq<BExpr>
//    | Sum of seq<'t NExpr>        



//type State = string * int
//and Expr = 
//    | StateNExpr of State NExpr
//    | VarNExpr of Var NExpr
//    | BExpr of BExpr    

//and 't Term = 
//    | BConst of bool 
//    | BVar of Var
//    | Gt  of 't NExpr * 't NExpr  //greater than 
//    | Ge  of 't NExpr * 't NExpr  //greater than or equal
//    | Lt  of 't NExpr * 't NExpr  //less than
//    | Le  of 't NExpr * 't NExpr  //less than or equal
//    | Eq  of 't NExpr * 't NExpr  //numerical equal             
//and BExpr =         
//    | StateTerm of State NExpr
//    | VarTerm of Var NExpr
//    | Beq of BExpr * BExpr  //logical equivalence
//    | Imp of BExpr * BExpr  //Implies
//    | And of BExpr * BExpr  //logical AND
//    | Or  of BExpr * BExpr  //logical OR
//    | LAnd of seq<BExpr>    //logical AND
//    | LOr  of seq<BExpr>    //logical OR
//    | Not of BExpr          //negation    
//    | Sat of State * string //application of a predicate to a given experiment at a given timestep        
//    //| Predicate of string * string * int  //application of a predicate to a given experiment at a given timestep        
// 



type Pred = 
    |ConcPred of string * string * int  //predicate name * experiment * time step
    |AbsKPred of string * string        //predicate name * experiment
    |AbsPPred of string * int           //predicate name * time step
    |AbsPred of string                  //predicate name
    override this.ToString() = 
        match this with
        | ConcPred(n, p, k) -> System.String.Format("({0}[{1}]|={2})",p,k,n)        
        | AbsKPred(n, p) -> System.String.Format("({0}|={1})",p,n)        
        | AbsPPred(n, k) -> System.String.Format("({0}[{1}])",n,k)        
        | AbsPred(n) -> System.String.Format("{0}",n)           
    member this.InitPath p = 
        match this with                  
        | AbsPPred(n, k) ->  ConcPred(n, p, k)
        | AbsPred(n) -> AbsKPred(n, p)
        | _ -> this
    member this.InitStep k = 
        match this with    
        | AbsKPred(n, p) -> ConcPred(n, p, k)
        | AbsPred(n) -> AbsPPred(n, k)
        | _ -> this    
    member this.Init p k = 
        let iP = this.InitPath p in
        iP.InitStep k
    member this.Name = 
        match this with
        | ConcPred(n, _, _) -> n
        | AbsKPred(n, _) -> n
        | AbsPPred(n, _) -> n
        | AbsPred(n) -> n
    member this.ExtractPath() = 
        match this with
        | ConcPred(_, p, k) -> (p,k)
        | _ -> failwith "Abstract predicates are not handled yet"

type Expr = 
    | NExpr of NExpr
    | BExpr of BExpr  
    override this.ToString() = 
        match this with
         | NExpr(e) -> e.ToString()
         | BExpr(e) -> e.ToString()  
    member this.ToLatex() = 
        match this with
         | NExpr(e) -> e.ToLatex()
         | BExpr(e) -> e.ToLatex()  
    member this.Vars() = 
        match this with
         | NExpr(e) -> e.Vars()
         | BExpr(e) -> e.Vars()
    member this.InitPath p = 
       match this with
       |NExpr(e) -> NExpr(e.InitPath p)
       |BExpr(e) -> BExpr(e.InitPath p)
    member this.InitStep k = 
       match this with
       |NExpr(e) -> NExpr(e.InitStep k)
       |BExpr(e) -> BExpr(e.InitStep k)
    member this.Init p k = 
        let iP = this.InitPath p in
        iP.InitStep k
    member this.Inline predicates = 
       match this with
       |NExpr(e) -> NExpr(e.Inline predicates)
       |BExpr(e) -> BExpr(e.Inline predicates)
    member this.Simplify() = 
       match this with
            |NExpr(e) -> NExpr(e.Simplify())
            |BExpr(e) -> BExpr(e.Simplify())

//Boolean expressions
and BTerm = 
    | BComp of CExpr    
    | BConst of bool    
    | BVar of Var
    | Predicate of Pred//string * string * int  //application of a predicate to a given experiment at a given timestep    
    | Fixpoint of string * int //fixpoint of a given path at a given step (the application of any update rule does not change the state)
    | Terminal of string * int //terminal state of a given path at a given step (none of the guards are enabled)
    override this.ToString() = 
        match this with
        | BComp(e) -> e.ToString()    
        | BConst(c) -> c.ToString()
        | BVar(v) -> v.ToString()        
        | Predicate(p) -> p.ToString()
        | Fixpoint(p,k) -> System.String.Format("(Fixpoint({0}[{1}]))",p,k)
        | Terminal(p,k) -> System.String.Format("(Terminal({0}[{1}]))",p,k)
    member this.ToLatex() = 
        match this with
        | BComp(e) -> e.ToLatex()    
        | BConst(c) -> if c then @"\top" else @"\bot"
        | BVar(v) -> v.ToLatex()        
        | Predicate(_) -> failwith "Predicate to latex not yet implemented"
        | Fixpoint(p,k) -> System.String.Format(@"(Fixpoint({0}[{1}]))",p,k)
        | Terminal(p,k) -> System.String.Format(@"(Terminal({0}[{1}]))",p,k)
    member this.VarNames() = 
        match this with
        | BComp(e) -> e.VarNames()
        | BConst(_) -> Seq.empty
        | BVar(v) -> v.Name |> Seq.singleton
        | Predicate _ -> failwith "Predicates must be inlined to extract variable names"       
        | Fixpoint _ -> Seq.empty
        | Terminal _ -> Seq.empty
    member this.Vars() = 
        match this with
        | BComp(e) -> e.Vars()
        | BConst(c) -> Seq.empty
        | BVar(v) -> v.Vars()        
        | Predicate _ -> failwith "Predicates must be inlined to extract variables"
        | Fixpoint _ -> Seq.empty
        | Terminal _ -> Seq.empty
    member this.InitPath p = 
        match this with
        | BComp(c) -> BComp(c.InitPath p)
        | BConst(c) -> BConst(c)
        | BVar(v) -> BVar(v.InitPath p)
        | Predicate(pred) -> Predicate(pred.InitPath p)
        | Fixpoint(p,k) -> Fixpoint(p,k) //BY: should probably have an abstract fixpoint to use in predicates?
        | Terminal(p,k) -> Terminal(p,k) //BY: should probably have an abstract fixpoint to use in predicates?
    member this.InitStep k = 
        match this with
        | BComp(c) -> BComp(c.InitStep k)
        | BConst(c) -> BConst(c)
        | BVar(v) -> BVar(v.InitStep k)        
        | Predicate(pred) -> Predicate(pred.InitStep k)
        | Fixpoint(p,k) -> Fixpoint(p,k) //BY: should probably have an abstract fixpoint to use in predicates?
        | Terminal(p,k) -> Terminal(p,k) //BY: should probably have an abstract fixpoint to use in predicates?
    member this.Init p k = 
        let iP = this.InitPath p in
        iP.InitStep k   
    member this.Simplify() = 
        match this with
        | BComp(c) -> BComp(c.Simplify())
        | _ -> this
    member this.ToFS() =     
        match this with
        | BComp(e) -> e.ToFS()    
        | BConst(c) -> if c then "true" else "false"
        | BVar(v) -> v.ToFS()        
        | Predicate(_) -> failwith "Exporting predicates to F# is not supported yet."
        | Fixpoint(_) -> failwith "Exporting fixpoint constraints to F# is not supported yet."
        | Terminal(_) -> failwith "Exporting terminal constraints to F# is not supported yet."
    
    member this.subst (vals:Map<string,string>) = 
        match this with
        | BComp(e) -> BComp(e.subst vals)
        | BVar(v) -> if vals.ContainsKey(v.Name) then BConst(vals.[v.Name].ToLower().Trim() = "true") else BVar(v)
        | _ -> failwith "substitution not implemented"
       
             
and BExpr =     
    | BTerm of BTerm
    | Beq of BExpr * BExpr  //logical equivalence
    | Imp of BExpr * BExpr  //Implies
    | And of BExpr * BExpr  //logical AND
    | Or  of BExpr * BExpr  //logical OR
    | LAnd of seq<BExpr>    //logical AND
    | LOr  of seq<BExpr>    //logical OR
    | Not of BExpr          //negation        
    override this.ToString() = 
        match this with                
        | BTerm(t) -> t.ToString()
        | Beq(e1,e2)->System.String.Format("({0}) <=> ({1})",e1.ToString(),e2.ToString()) 
        | Imp(e1,e2)->System.String.Format("({0}) => ({1})",e1.ToString(),e2.ToString()) 
        | And(e1,e2)->System.String.Format("({0}) and ({1})",e1.ToString(),e2.ToString()) 
        | Or(e1,e2)->System.String.Format("({0}) or ({1})",e1.ToString(),e2.ToString()) 
        | LAnd(el)->
            let n = Seq.length el in
            if n=0 then ""            
            else 
                el
                |> Seq.map(fun e -> e.ToString() |> sprintf "(%s)")
                |> String.concat " and "

        | LOr(el)->
            let n = Seq.length el in
            if n=0 then ""
            else 
                el
                |> Seq.map(fun e -> e.ToString() |> sprintf "(%s)")
                |> String.concat " or "

        | Not(e)->System.String.Format("not ({0})",e.ToString())         
     member this.ToLatex() = 
        match this with                
        | BTerm(t) -> t.ToLatex()
        | Beq(e1,e2)->System.String.Format(@"({0}) \Leftrightarrow ({1})",e1.ToLatex(),e2.ToLatex()) 
        | Imp(e1,e2)->System.String.Format(@"({0}) \Rightarrow ({1})",e1.ToLatex(),e2.ToLatex()) 
        | And(e1,e2)->System.String.Format(@"({0}) \wedge ({1})",e1.ToLatex(),e2.ToLatex()) 
        | Or(e1,e2)->System.String.Format(@"({0}) \vee ({1})",e1.ToLatex(),e2.ToLatex()) 
        | LAnd(el)->
            let el = Seq.map(fun (e:BExpr) -> e.ToLatex()) el in
            let n = Seq.length el in
            if n=0 then ""
            else if n=1 then System.String.Format("({0})",Seq.head el) 
            else if n=2 then System.String.Format(@"({0} \wedge {1})",Seq.head el, Seq.head (Seq.skip(1) el)) 
            else System.String.Format(@"\bigwedge({0})",Lib.seq2str el) 
        | LOr(el)->
            let el = Seq.map(fun (e:BExpr) -> e.ToLatex()) el in
            let n = Seq.length el in
            if n=0 then ""
            else if n=1 then System.String.Format("({0})",Seq.head el) 
            else if n=2 then System.String.Format(@"({0} \vee {1})",Seq.head el, Seq.head (Seq.skip(1) el)) 
            else System.String.Format(@"\bigvee({0})",Lib.seq2str el) 
        | Not(e)->System.String.Format(@"\neg({0})",e.ToLatex()) 

    member this.map f = 
        match this with
        | BTerm(term) -> f term
        | Beq(e1,e2) -> Beq(e1.map f, e2.map f)
        | Imp(e1,e2) -> Imp(e1.map f, e2.map f)
        | And(e1,e2) -> And(e1.map f, e2.map f)
        | Or(e1,e2) -> Or(e1.map f, e2.map f)
        | LAnd(el) -> LAnd(Seq.map(fun (e:BExpr) -> e.map f) el)
        | LOr(el) -> LOr(Seq.map(fun (e:BExpr) -> e.map f) el)
        | Not(e) -> Not(e.map f)         
    member this.fold f = 
        match this with
        | BTerm(term) -> f term
        | Beq(e1,e2) -> Seq.append(e1.fold f) (e2.fold f)
        | Imp(e1,e2) -> Seq.append(e1.fold f) (e2.fold f)
        | And(e1,e2) -> Seq.append(e1.fold f) (e2.fold f)
        | Or(e1,e2) -> Seq.append(e1.fold f) (e2.fold f)
        | LAnd(el) -> Seq.collect(fun (e: BExpr) -> e.fold f) el
        | LOr(el) -> Seq.collect(fun (e: BExpr) -> e.fold f) el
        | Not(e) -> e.fold f
                              
    member this.VarNames() = this.fold (fun term -> term.VarNames())
    member this.Vars() = this.fold (fun term -> term.Vars())
    member this.InitPath p = this.map (fun term -> BTerm(term.InitPath p))        
    member this.InitStep k = this.map (fun term -> BTerm(term.InitStep k))        
    member this.Init p k = let iP = this.InitPath p in iP.InitStep k
    member this.Inline (predicates:Map<string,BExpr>) = 
        let inliner term =        
            match term with   
            | Predicate(pred) -> 
                match pred with
                | ConcPred(n,p,k) -> (predicates.[n].Init p k).Inline predicates
                | _ -> failwith "Cannot inline predicates that are not initialized."                                
//            | Fixpoint(_) -> failwith "Fixpoint constraints cannot be inlined directly. Use the appropriate tactic first"  
//            | Terminal(_) -> failwith "Terminal constraints cannot be inlined directly. Use the appropriate tactic first"  
            | _ -> BTerm(term)
        in                
        this.map inliner
    member this.ToFS() = 
        match this with                
        | BTerm(t) -> t.ToFS()
        | Beq(e1,e2)->System.String.Format("({0}) = ({1})",e1.ToFS(),e2.ToFS()) 
        | Imp(e1,e2)->System.String.Format("({0}) => ({1})",e1.ToFS(),e2.ToFS()) 
        | And(e1,e2)->System.String.Format("({0}) && ({1})",e1.ToFS(),e2.ToFS()) 
        | Or(e1,e2)->System.String.Format("({0}) || ({1})",e1.ToFS(),e2.ToFS()) 
        | LAnd(el)->
            if Seq.isEmpty el then "true"
            else el |> Seq.map(fun e -> e.ToFS()) |> Seq.reduce(fun a b -> System.String.Format("({0}) && ({1})",a,b))  
        | LOr(el)->
            if Seq.isEmpty el then "true"
            else el |> Seq.map(fun e -> e.ToFS()) |> Seq.reduce(fun a b -> System.String.Format("({0}) || ({1})",a,b))  
        | Not(e)->System.String.Format("not ({0})",e.ToFS())      
    member this.ToFSFn(name) =     
        let vars = this.VarNames()
        let param = (if Seq.isEmpty vars then "" else (vars |> Seq.distinct |> Seq.reduce(fun a b -> a + ", " + b)))
        "let " + name + "(" + param + ") = " + "(" + this.ToFS()     + ")\n" + 
        "let "+ name+"_str (" + param + ")= (" + name + "(" + param + ")).ToString()"

    member this.subst (vals:Map<string,string>) = 
        this.map (fun t -> BTerm(t.subst vals))
    

   //NOTE: FOR NOW, THIS FUNCTION IS INTENDED ONLY TO PRODUCE SMALLER TEXTUAL REPRESENTATIONS OF MODELS
    member this.Simplify() = 
       match this with       
       | Not(BTerm(BConst(e))) -> BTerm(BConst(not e))    //NOT b rewrite
       | Not(Not(e)) -> e.Simplify()  //NOT (NOT A)) := A
       | Not(e) -> Not(e.Simplify()) 
       | And(e1,e2) when e1=e2 -> e1.Simplify()
       | Or(e1,e2) when e1=e2 -> e1.Simplify()        
       | And(e1,e2) when e1.Simplify()=BTerm(BConst(false)) || e2.Simplify()=BTerm(BConst(false)) -> BTerm(BConst(false))
       | And(e1,e2) when e1.Simplify()=BTerm(BConst(true)) -> e2.Simplify()
       | And(e1,e2) when e2.Simplify()=BTerm(BConst(true)) -> e1.Simplify()
       | Or(e1,e2) when e1.Simplify()=BTerm(BConst(true)) || e2.Simplify()=BTerm(BConst(true)) -> BTerm(BConst(true))
       | Or(e1,e2) when e1.Simplify()=BTerm(BConst(false)) -> e2.Simplify()
       | Or(e1,e2) when e2.Simplify()=BTerm(BConst(false)) -> e1.Simplify()
       | Imp(e1, e2) when e1.Simplify()=BTerm(BConst(true))-> e2.Simplify() //True => p := p 
       | Imp(e1, e2) when e1.Simplify()=BTerm(BConst(false))-> BTerm(BConst(true)) //false => p := true 
       | Imp(e1, e2) when e2.Simplify()=BTerm(BConst(false))-> Not(e1).Simplify() //p => False := NOT p    
       | Imp(e1, e2) when e2.Simplify()=BTerm(BConst(true))-> BTerm(BConst(true)) //p => true := true    
       | BTerm(t) -> BTerm(t.Simplify()) 
       | Beq(e1,e2) -> Beq(e1.Simplify(), e2.Simplify())
       | Imp(e1,e2) -> Imp(e1.Simplify(), e2.Simplify())
       | And(e1,e2) -> And(e1.Simplify(), e2.Simplify())
       | Or(e1,e2) -> Or(e1.Simplify(), e2.Simplify())
       | LAnd(el) -> 
            let clauses = 
                Seq.map(fun (e:BExpr) -> e.Simplify()) el          
                |> Seq.filter (fun e -> e<>BTerm(BConst(true)))
            if Seq.isEmpty clauses then 
                BTerm(BConst(true))
            elif Seq.exists (fun c -> c = BTerm(BConst(false))) clauses then 
                BTerm(BConst(false))
            else
                LAnd(clauses)            
       | LOr(el) -> 
            let clauses = 
                Seq.map(fun (e:BExpr) -> e.Simplify()) el          
                |> Seq.filter (fun e -> e<>BTerm(BConst(false)))
            if Seq.isEmpty clauses then 
                BTerm(BConst(false))
            elif Seq.exists (fun c -> c = BTerm(BConst(true))) clauses then 
                BTerm(BConst(true))
            else
                LOr(clauses)  
       | Not(e) -> Not(e.Simplify())
       | _ -> this
//    static member op_BooleanAnd (a:BExpr, b:BExpr) =
//        And(a,b)
//    static member op_BooleanOr (a:BExpr, b:BExpr) =
//        Or(a,b)   

//Numerical (arithmetic) expression    
and NTerm = 
    | NConst of int   
    | NVar of Var 
    | Ite of BExpr * NExpr * NExpr
    | Card of seq<BExpr>        
    override this.ToString() =
        match this with 
        | NConst(n)->n.ToString()
        | NVar(v) -> v.ToString()
        | Ite(c,e1,e2)->System.String.Format("if ({0}) then ({1}) else ({2})",c.ToString(),e1.ToString(),e2.ToString()) 
        | Card(es) -> 
            let es_str = 
                if Seq.isEmpty es then ""  
                else
                    Seq.map(fun e -> e.ToString())  es
                    |> Seq.reduce(fun a b -> a + ", " + b) in            
            System.String.Format("card({0})", es_str)      
    member this.ToLatex() =
        match this with 
        | NConst(n)->n.ToString()
        | NVar(v) -> v.ToLatex()
        | Ite(c,e1,e2)->System.String.Format("if ({0}) then ({1}) else ({2})",c.ToLatex(),e1.ToLatex(),e2.ToLatex()) 
        | Card(es) -> 
            let es_str = 
                if Seq.isEmpty es then ""  
                else
                    Seq.map(fun (e:BExpr) -> e.ToLatex())  es
                    |> Seq.reduce(fun a b -> a + ", " + b) in            
            System.String.Format("||{0}||", es_str)            
    member this.VarNames() =
        match this with 
        | NConst(n)->Seq.empty
        | NVar(v) ->v.Name |> Seq.singleton
        | Ite(c,e1,e2)->Seq.append(c.VarNames()) (Seq.append(e1.VarNames()) (e2.VarNames()))
        | Card(es) -> Seq.fold(fun acc (e:BExpr) -> Seq.append(acc) (e.VarNames())) Seq.empty es
    member this.Vars() =
        match this with 
        | NConst(n)->Seq.empty
        | NVar(v) ->v.Vars()    
        | Ite(c,e1,e2)->Seq.append(c.Vars()) (Seq.append(e1.Vars()) (e2.Vars()))
        | Card(es) -> Seq.fold(fun acc (e:BExpr) -> Seq.append(acc) (e.Vars())) Seq.empty es        
    member this.InitPath p = 
        match this with
        | NConst n -> NConst(n)
        | NVar(v) -> NVar(v.InitPath p)    
        | Ite(c,e1,e2)-> Ite(c.InitPath p,e1.InitPath p, e2.InitPath p)
        | Card(es)  -> Card(Seq.map(fun (e:BExpr) -> e.InitPath p) es)        
    member this.InitStep k = 
        match this with
        | NConst n -> NConst(n)
        | NVar(v) -> NVar(v.InitStep k)        
        | Ite(c,e1,e2)-> Ite(c.InitStep k,e1.InitStep k, e2.InitStep k)            
        | Card(es)  -> Card(Seq.map(fun (e:BExpr) -> e.InitStep k) es)        
    member this.Init p k = 
        let iP = this.InitPath p in
        iP.InitStep k
    member this.Inline predicates = 
        match this with
        | NConst n -> NConst(n)
        | NVar(v) -> NVar(v)    
        | Ite(c,e1,e2)-> Ite(c.Inline predicates,e1.Inline predicates, e2.Inline predicates)        
        | Card(es)  -> Card(Seq.map(fun (e:BExpr) -> e.Inline predicates) es)        
    member this.Simplify() = 
        match this with
        | Ite(c,e1,e2) -> Ite(c.Simplify(),e1.Simplify(),e2.Simplify())
        | Card(es) -> Card(Seq.map(fun (e:BExpr) -> e.Simplify()) es)
        | _ -> this
    member this.ToFS() = 
        match this with 
        | NConst(n)->n.ToString()
        | NVar(v) -> v.ToFS()
        | Ite(c,e1,e2)->System.String.Format("if ({0}) then ({1}) else ({2})",c.ToFS(),e1.ToFS(),e2.ToFS()) 
        | Card(es) -> failwith "Cardinality export to F# is not supported yet"
            //es |> Seq.map (fun e -> e.ToFS())
                
and NExpr =              
    | NTerm of NTerm
    | Neg of NExpr
    | Add of NExpr * NExpr
    | Sub of NExpr * NExpr
    | Mul of NExpr * NExpr    
    override this.ToString() =
        match this with         
        | NTerm(t)->t.ToString()
        | Neg(e)->System.String.Format("-{0}",e.ToString()) 
        | Add(e1,e2)->System.String.Format("({0}) + ({1})",e1.ToString(),e2.ToString()) 
        | Sub(e1,e2)->System.String.Format("({0}) - ({1})",e1.ToString(),e2.ToString()) 
        | Mul(e1,e2)->System.String.Format("({0}) * ({1})",e1.ToString(),e2.ToString())         
    member this.ToLatex() =
        match this with         
        | NTerm(t)->t.ToLatex()
        | Neg(e)->System.String.Format("-{0}",e.ToLatex()) 
        | Add(e1,e2)->System.String.Format("({0}) + ({1})",e1.ToLatex(),e2.ToLatex()) 
        | Sub(e1,e2)->System.String.Format("({0}) - ({1})",e1.ToLatex(),e2.ToLatex()) 
        | Mul(e1,e2)->System.String.Format("({0}) * ({1})",e1.ToLatex(),e2.ToLatex())    
                
    member this.map f = 
        match this with         
        | NTerm(t)-> f t
        | Neg(e)-> Neg(e.map f)
        | Add(e1,e2)-> Add(e1.map f, e2.map f)
        | Sub(e1,e2)-> Sub(e1.map f, e2.map f)
        | Mul(e1,e2)-> Mul(e1.map f, e2.map f)                    
    member this.fold f = 
        match this with         
        | NTerm(t)-> f t
        | Neg(e)-> e.fold f
        | Add(e1,e2)-> Seq.append(e1.fold f) (e2.fold f)
        | Sub(e1,e2)-> Seq.append(e1.fold f) (e2.fold f)
        | Mul(e1,e2)-> Seq.append(e1.fold f) (e2.fold f)          
    member this.VarNames() = this.fold (fun term -> term.VarNames())
    member this.Vars() = this.fold (fun term -> term.Vars())
    member this.InitPath p = this.map (fun term -> NTerm(term.InitPath p))        
    member this.InitStep k = this.map (fun term -> NTerm(term.InitStep k))        
    member this.Init p k = let iP = this.InitPath p in iP.InitStep k
    member this.Inline (predicates:Map<string,BExpr>) = this.map (fun term -> NTerm(term.Inline predicates))      
    member this.Simplify() =  
        match this with
        | Add(NTerm(NConst(n1)), NTerm(NConst(n2))) -> NTerm(NConst(n1 + n2))
        | Sub(NTerm(NConst(n1)), NTerm(NConst(n2))) -> NTerm(NConst(n1 - n2))
        | Mul(NTerm(NConst(n1)), NTerm(NConst(n2))) -> NTerm(NConst(n1 * n2))
        | Neg(NTerm(NConst(0))) -> NTerm(NConst(0))
        | Neg(Neg(e)) -> e.Simplify()
        | Add(e, NTerm(NConst(0))) -> e.Simplify()
        | Add(NTerm(NConst(0)), e) -> e.Simplify()
        | Add(NTerm(NConst(n)), e) -> Add(e, NTerm(NConst(n))).Simplify() //TODO: why is this needed?
        | Add(e1, Neg(e2)) -> Sub(e1, e2).Simplify()
        | Add(Neg(e1), e2) -> Sub(e2, e1).Simplify()
        | Sub(e, NTerm(NConst(0))) -> e.Simplify()
        | Sub(NTerm(NConst(0)), e) -> Neg(e).Simplify()
        | Mul(e, NTerm(NConst(1))) -> e.Simplify()
        | Mul(NTerm(NConst(1)), e) -> e.Simplify()
        | Mul(e, NTerm(NConst(0))) -> NTerm(NConst(0))
        | Mul(NTerm(NConst(0)), e) -> NTerm(NConst(0))
        | Mul(e, NTerm(NConst(n))) -> Mul(NTerm(NConst(n)), e).Simplify() //TODO: why is this needed?
        | Mul(Neg(e1), e2) -> Neg(Mul(e1, e2)).Simplify()
        | Mul(e1, Neg(e2)) -> Neg(Mul(e1, e2)).Simplify()
        | Add(e1,e2) -> Add(e1.Simplify(),e2.Simplify())
        | Sub(e1,e2) -> Add(e1.Simplify(),e2.Simplify())
        | NTerm(t) -> NTerm(t.Simplify())
        | _ -> this  
    member this.ToFS() =         
            match this with         
            | NTerm(t)->t.ToString()
            | Neg(e)->System.String.Format("-{0}",e.ToFS()) 
            | Add(e1,e2)->System.String.Format("({0}) + ({1})",e1.ToFS(),e2.ToFS()) 
            | Sub(e1,e2)->System.String.Format("({0}) - ({1})",e1.ToFS(),e2.ToFS()) 
            | Mul(e1,e2)->System.String.Format("({0}) * ({1})",e1.ToFS(),e2.ToFS())         
    member this.ToFSFn(name) =     
        let vars = this.VarNames()
        "let " + name + "(" + 
        (if Seq.isEmpty vars then "" else (vars |> Seq.distinct |> Seq.reduce(fun a b -> a + ", " + b))) + 
        ") = " + this.ToFS()
    member this.subst (vals:Map<string,string>) = 
        this.map (fun t -> 
            match t with 
            | NVar(v) -> if vals.ContainsKey(v.Name) then NTerm(NConst(System.Int32.Parse(vals.[v.Name]))) else NTerm(t)            
            | Ite(a,b,c) -> NTerm(Ite(a.subst vals, b.subst vals, c.subst vals))
            | Card(se) -> NTerm(Card(se |> Seq.map(fun e -> e.subst vals)))
            | NConst(n) -> NTerm(NConst(n))
            )
        
        


//    static member (+) (a:NExpr, b:NExpr) =
//        Add(a,b)
//    static member (-) (a:NExpr, b:NExpr) =
//        Sub(a,b)
//    static member (*) (a:NExpr, b:NExpr) =
//        Mul(a,b)
//    

//Compare expressions
and CExpr =             
    | Gt  of NExpr * NExpr  //greater than 
    | Ge  of NExpr * NExpr  //greater than or equal
    | Lt  of NExpr * NExpr  //less than
    | Le  of NExpr * NExpr  //less than or equal
    | Eq  of NExpr * NExpr  //numerical equal      
    
    override this.ToString() =
        match this with 
        | Gt(e1,e2)->System.String.Format("({0}) > ({1})",e1.ToString(),e2.ToString()) 
        | Ge(e1,e2)->System.String.Format("({0}) >= ({1})",e1.ToString(),e2.ToString()) 
        | Lt(e1,e2)->System.String.Format("({0}) < ({1})",e1.ToString(),e2.ToString()) 
        | Le(e1,e2)->System.String.Format("({0}) <= ({1})",e1.ToString(),e2.ToString()) 
        | Eq(e1,e2)->System.String.Format("({0}) = ({1})",e1.ToString(),e2.ToString()) 
    
    member this.ToLatex() =
        match this with 
        | Gt(e1,e2)->System.String.Format("({0}) > ({1})",e1.ToLatex(),e2.ToLatex()) 
        | Ge(e1,e2)->System.String.Format(@"({0}) \geq({1})",e1.ToLatex(),e2.ToLatex()) 
        | Lt(e1,e2)->System.String.Format("({0}) < ({1})",e1.ToLatex(),e2.ToLatex()) 
        | Le(e1,e2)->System.String.Format(@"({0}) \leq ({1})",e1.ToLatex(),e2.ToLatex()) 
        | Eq(e1,e2)->System.String.Format("({0}) = ({1})",e1.ToLatex(),e2.ToLatex()) 

    member this.map f = 
        match this with 
        | Gt(e1,e2) -> Gt(f e1, f e2)
        | Ge(e1,e2) -> Ge(f e1, f e2)
        | Lt(e1,e2) -> Lt(f e1, f e2)
        | Le(e1,e2) -> Le(f e1, f e2)
        | Eq(e1,e2) -> Eq(f e1, f e2)       
    member this.fold f = 
        match this with 
        | Gt(e1,e2) -> Seq.append(f e1) (f e2)
        | Ge(e1,e2) -> Seq.append(f e1) (f e2)
        | Lt(e1,e2) -> Seq.append(f e1) (f e2)
        | Le(e1,e2) -> Seq.append(f e1) (f e2)
        | Eq(e1,e2) -> Seq.append(f e1) (f e2)
     member this.VarNames() = this.fold (fun (e:NExpr) -> e.VarNames())         
     member this.Vars() =     this.fold (fun (e:NExpr) -> e.Vars())         
     member this.InitPath p = this.map (fun (e:NExpr) -> e.InitPath p)
     member this.InitStep k = this.map (fun (e:NExpr) -> e.InitStep k)
     member this.Init p k = 
        let iP = this.InitPath p in
        iP.InitStep k
     member this.Simplify() = this.map (fun (e:NExpr) -> e.Simplify())
//     static member op_GreaterThan (a:NExpr, b:NExpr) =
//        Gt(a,b)
//     static member op_GreaterThanOrEqual (a:NExpr, b:NExpr) =
//        Ge(a,b)
//     static member op_LessThan (a:NExpr, b:NExpr) =
//        Lt(a,b)
//     static member op_LessThanOrEqual (a:NExpr, b:NExpr) =
//        Le(a,b)
//     static member op_Equality (a:NExpr, b:NExpr) =
//        Eq(a,b)
//    
    member this.ToFS() = 
        match this with 
        | Gt(e1,e2)->System.String.Format("({0}) > ({1})",e1.ToFS(),e2.ToFS()) 
        | Ge(e1,e2)->System.String.Format("({0}) >= ({1})",e1.ToFS(),e2.ToFS()) 
        | Lt(e1,e2)->System.String.Format("({0}) < ({1})",e1.ToFS(),e2.ToFS()) 
        | Le(e1,e2)->System.String.Format("({0}) <= ({1})",e1.ToFS(),e2.ToFS()) 
        | Eq(e1,e2)->System.String.Format("({0}) = ({1})",e1.ToFS(),e2.ToFS()) 

    member this.subst (vals:Map<string,string>) =         
        this.map (fun (t:NExpr) -> t.subst vals)
        
        


//export all variables from all specifications
let GetAllVars spec =  Seq.fold(fun acc (x:BExpr)-> 
                        Seq.append(acc) (x.Vars())) Seq.empty spec                             

//construct a list of paths together with the max. timestep of each one
//NOTE: assuming that all labels are already removed
let GetAllPaths spec = 
    let rec extractNpath e = 
        match e with         
        | NVar(v) -> match v with Var.StateVar(p,k,_) -> (p,k) |> Seq.singleton | _ -> Seq.empty
        | Ite(c,e1,e2) -> Seq.append(c.fold extractBpath) (Seq.append(e1.fold extractNpath) (e2.fold extractNpath))
        | Card(es) -> Seq.collect(fun (e:BExpr) -> e.fold extractBpath) es
        | _ -> Seq.empty
    and extractBpath e = 
        match e with
        | Predicate(pred) ->   pred.ExtractPath() |> Seq.singleton
        | BVar(v) -> match v with Var.StateVar(p,k,_) -> (p,k) |> Seq.singleton | _ -> Seq.empty
        | BComp(e) -> e.fold(fun se -> se.fold extractNpath)
        | _ -> Seq.empty
    in
    
    Seq.fold(fun acc (e:BExpr) -> Seq.append(acc) (e.fold extractBpath)) Seq.empty spec
    |> Seq.groupBy(fun (n,k) -> n) 
    |> Seq.map(fun (n,S) -> 
        let sq =  Seq.map(fun (x,k) -> k) S                                                       
        let max = sq |> Seq.max in
        let min = sq |> Seq.min in
        n,min,max)

    
//    Seq.fold(fun acc i-> match i with 
//                                            | Var.StateVar(p,k,v) -> Seq.append(acc) (Seq.singleton (p,k))
//                                            | _ -> acc ) Seq.empty (GetAllVars spec)
//                        
//                        
//                        
//                        |> Seq.groupBy(fun (n,k) -> n) 
//                        |> Seq.map(fun (n,S) -> 
//                            let sq =  Seq.map(fun (x,k) -> k) S                                                       
//                            let max = sq |> Seq.max in
//                            let min = sq |> Seq.min in
//                            n,min,max)




let Simplify (e:Expr) = 
     let mutable se = e in
     while (not (se.Equals(e.Simplify()))) do
            se <- se.Simplify()
     se

let BSimplify (e:BExpr) = 
     let mutable se = e in
     while (not (se.Equals(se.Simplify()))) do
            se <- se.Simplify()
     se


//let MkBConst c = BTerm(BConst(c))
//let MkBVar v = BTerm(BVar(v))
//let MkNConst c = NTerm(NConst(c))
//let MkNVar v = NTerm(NVar(v))
//let MkEq(v,c) = BTerm(BComp(Eq(MkNVar v,MkNConst c)))