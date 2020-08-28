module Microsoft.Research.ReasoningEngine.Solution

open System

type State = Map<string,string> //key: variable name (string); value: variable value (string)



let lin2str (lines:Map<'a,string>) = if Map.isEmpty lines then "" else (Map.toSeq lines) |> Seq.map snd |> Seq.reduce(fun a b -> a + "\n" + b)
let lin2strnl (lines:Map<'a,string>) = if Map.isEmpty lines then "" else (Map.toSeq lines) |> Seq.map snd |> Seq.reduce(fun a b -> a + ", " + b)
type Path = {
    name: string;
    states:  Map<int,State>;
    vars: Map<string,string>;
    } with
    override this.ToString() = 
        String.Format(" Trajectory: {0}\n", this.name) + 
        (Map.map(fun n v -> String.Format("  {0}={1}",n,v)) this.vars |> lin2str) + 
        (Map.map(fun k s -> String.Format("   [{0}] {1}",k,Map.map(fun n v -> n + "=" + v) s |> lin2strnl)) this.states |> lin2str)

type Solution = {
    paths: Map<string,Path>;
    vars: Map<string,string>;
    nondet: Map<string,string>;    
    varmap: Map<Var.Var,string>;    
    } with
    override this.ToString() = 
     (Map.map(fun n v -> String.Format("{0}={1}",n,v)) this.vars |> lin2str) + "\n" + 
     (Map.map(fun n p -> p.ToString()) this.paths |> lin2str)
                                    
    static member Construct(V:Map<Var.Var,string>) = 
        let (state_vars,path_vars,sys_vars) = 
            V |> Map.toSeq 
            |> Seq.fold(fun (sv,pv,ssv) (v,vv) -> 
                                    match v with 
                                    |Var.StateVar(p,k,n)  -> ((p,k,n,vv)::sv,pv,ssv)
                                    |Var.PathVar(p,n) ->  (sv,(p,n,vv)::pv,ssv)
                                    |Var.SysVar(n) ->  (sv,pv,(n,vv)::ssv)                   
                                    | _ -> failwith "Variables must be concrete!"
                        ) (List.empty,List.empty, List.empty) in


         //group all state variables
         //Map<string*Map<int*Map<string,string>>>: path_name -> step -> var_name -> value
         let state_vars =               
            state_vars 
            |> Seq.groupBy(fun (p,_,_,_) -> p ) |> Map.ofSeq
            |> Map.map(fun _ s -> Seq.groupBy(fun (_,k,_,_) -> k) s 
                                  |> Map.ofSeq
                                  |> Map.map(fun _ s2 -> Seq.map(fun (_,_,n,v) -> n,v) s2 |> Map.ofSeq))                                 
         
         //group all path variables
         let path_vars = 
            Seq.groupBy(fun (p,n,v) -> p) path_vars          
            |> Seq.map(fun (p,vs) -> p,Seq.map(fun (_,v,vv) -> (v,vv)) vs |> Map.ofSeq)
            |> Map.ofSeq
         in
            
         //extract all path names
         let path_names = Seq.append(Seq.map fst (Map.toSeq path_vars)) (Seq.map fst (Map.toSeq state_vars)) |> Seq.distinct in

         //construct the path structures
         let paths = Seq.map(fun n -> n,{name = n; 
                                         states = if state_vars.ContainsKey n then state_vars.[n] else Map.empty; 
                                         vars = if path_vars.ContainsKey n then path_vars.[n] else Map.empty
                                         }) path_names |> Map.ofSeq in

         //return a solution structure
         {paths=paths; vars = sys_vars |> Map.ofList; nondet = Map.empty; varmap = V}
        
        
        