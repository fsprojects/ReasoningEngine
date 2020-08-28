module Microsoft.Research.ReasoningEngine.Var

type Type = 
    | Int               //integer
    | BInt of int       //bounded integer
    | Nat               //non-negative integer
    | BNat of int       //bounded non-negative integer
    | Bool              //Boolean
    override this.ToString() = 
        match this with
        | Int -> "int"
        | BInt n -> "int[" + n.ToString() + "]"
        | Nat -> "nat"
        | BNat n -> "nat[" + n.ToString() + "]"
        | Bool ->  "bool"
    member this.ToLatex() = 
        match this with
        | Int -> "\mathcal{Z}"
        | BInt n -> "\mathcal{Z}^{\leq " + n.ToString() + "}"
        | Nat -> "\mathcal{N}"
        | BNat n -> "\mathcal{N}^{0\ldots" + n.ToString() + "}"
        | Bool ->  "\mathcal{B}"
type Scope = 
    | State
    | Path
    | System
    override this.ToString() = 
        match this with
        | State -> "state"
        | Path  -> "path"
        | System  -> "system"
    
type VarDef = {
    scope   : Scope;
    t       : Type;
    name    : string;
    unique  : bool
    } with
    override this.ToString() =
        this.scope.ToString() + " " +  this.t.ToString() + " " + this.name + ";"
    member this.ToLatex() = this.name + @"\in" + this.t.ToLatex()
    static member DeclareVar(scope,t,name) = {scope=scope; t = t; name=name; unique = false}
    static member DeclareUniqueVar(scope,t,name) = {scope=scope; t = t; name=name; unique = true}

type Var = 
    | SysVar of string                      //system variable: e.g. a_represses_b (bool)
    | PathVar of string * string            //path variable: e.g. #experiment1.temperature  (path * var_name)
    | StateVar of string * int * string     //state variable: e.g. #experiment1[10].expression (path * k * var_name)
    | AbsPathVar of string                  //abstract path variable: #some_experiment.temperature (var_name)
    | AbsKStateVar of string * int * string //abstract path state variable: #experiment1[k-j].expression (path * j * var_name)
    | AbsPStateVar of int * string          //abstract step state variable: #some_experiment[10].expression (k * var_name)
    | AbsStateVar of int * string           //abstract state variable: #some_experiment[k-j].expression (j * var_name)
   
    override this.ToString() = 
        match this with 
        | SysVar(v) -> v
        | PathVar(p,v) -> System.String.Format("{0}.{1}",p,v)
        | StateVar(p,k,v) -> System.String.Format("{0}[{1}].{2}",p,k,v)
        | AbsPathVar(v) -> System.String.Format("p.{0}",v)
        | AbsKStateVar(p,n,v)-> if n=0 then System.String.Format("{0}[k].{1}",p,v) else System.String.Format("{0}[k{1}].{2}",p,n,v)
        | AbsPStateVar(k,v) -> System.String.Format("p[{0}].{1}",k,v)
        | AbsStateVar(n,v)  -> if n=0 then System.String.Format("p[k].{0}",v) else System.String.Format("p[k{0}].{1}",n,v)
    
    member this.ToLatex() = 
        match this with 
        | SysVar(v) -> v
        | PathVar(p,v) -> System.String.Format("{0}.{1}",p,v)
        | StateVar(p,k,v) -> System.String.Format("{0}[{1}].{2}",p,k,v)
        | AbsPathVar(v) -> System.String.Format("p.{0}",v)
        | AbsKStateVar(p,n,v)-> if n=0 then System.String.Format("{0}[k].{1}",p,v) else System.String.Format("{0}[k{1}].{2}",p,n,v)
        | AbsPStateVar(k,v) -> System.String.Format("p[{0}].{1}",k,v)
        | AbsStateVar(n,v)  -> if n=0 then System.String.Format("p[k].{0}",v) else System.String.Format("p[k{0}].{1}",n,v)

    //extract the variable (only concrete variables)
    member this.Vars() = 
        match this with 
        | SysVar(v) ->  SysVar(v) |> Seq.singleton
        | PathVar(p,v) ->PathVar(p,v) |> Seq.singleton
        | StateVar(p,k,v) -> StateVar(p,k,v) |> Seq.singleton
        | _ -> Seq.empty

    //initialize the path of a variable
    member this.InitPath p = 
        match this with    
        | AbsPathVar(s) -> PathVar(p,s)    
        | AbsPStateVar(k,s) ->StateVar(p,k,s)
        | AbsStateVar(j,s) -> AbsKStateVar(p,j,s)                
        | _ -> this

    //initialize the timestep of a variable
    member this.InitStep k = 
        match this with    
        | AbsKStateVar(p,j,s) -> StateVar(p,k+j,s) //note that the delay j is added to the timestep. This is caused by storing the delays as negative integers 
        | AbsStateVar(j,s) -> AbsPStateVar(k+j,s)  // --||--
        | _ -> this

    //initialize the variable with a path and timestep
    member this.Init p k =
        let iP = this.InitPath p in
        iP.InitStep k

    //extract the variable name
    member this.Name = 
        match this with 
        | SysVar v           ->  v
        | PathVar(_,v)       -> v
        | StateVar(_,_,v)    -> v
        | AbsPathVar v       -> v
        | AbsKStateVar(_,_,v)-> v
        | AbsPStateVar(_,v)  -> v
        | AbsStateVar(_,v)   -> v

    member this.Time = 
        match this with 
        | StateVar(_,k,_)   -> Some k
        | _                 -> None

    member this.Delay =
        match this with 
        | AbsKStateVar(_,j,_)-> Some(j)
        | AbsStateVar(j,_)  -> Some(j)
        | _ -> None


    //apply a function f to the variable
    member this.apply f = f this

    member this.ToFS() =      
        match this with 
        | SysVar(v) -> v
        | PathVar(p,v) -> System.String.Format("paths.[{0}].{1}",p,v)
        | StateVar(p,k,v) -> System.String.Format("paths.[{0}].[{1}].{2}",p,k,v)
        | AbsPathVar(v) -> System.String.Format("paths.[p].{0}",v)
        | AbsKStateVar(p,n,v)-> if n=0 then System.String.Format("paths.[{0}].[k].{1}",p,v) else System.String.Format("paths.[{0}].[k-{1}].{2}",p,n,v)
        | AbsPStateVar(k,v) -> System.String.Format("paths.[p].[{0}].{1}",k,v)
        | AbsStateVar(n,v)  -> if n=0 then System.String.Format("paths.[p].[k].{0}",v) else System.String.Format("paths.[p].[k-{0}].{1}",n,v)

//active pattern matching for abstract and concrete variables
let (|ConcVar|AbsVar|) var = 
    match var with 
    | SysVar(v) ->  ConcVar(var)
    | PathVar(p,v) -> ConcVar(var)
    | StateVar(p,k,v) -> ConcVar(var)
    | _ -> AbsVar(var)

//let (|VarName|) var = 
//    match var with 
//    | SysVar(v) ->  v
//    | PathVar(p,v) -> v
//    | StateVar(p,k,v) -> v
//    | AbsPathVar(v) -> v
//    | AbsKStateVar(p,n,v)-> v
//    | AbsPStateVar(k,v) -> v
//    | AbsStateVar(n,v)  -> v