module Microsoft.Research.REIN.Generator


type genSettings = {
    num_components: int;
    num_interactions: int;
    num_optional: int;
    num_experiments: int;
    num_components_in_experiment: int;
    valid_only: bool  //are the generated models satisfiable
    }   with
    override this.ToString() = 
        sprintf "Genes:\t\t%i\nInteractions:\t%i\nOptional interactions:\t%i\nExperiments:\t%i\nGenes in each experiment:\t%i" this.num_components this.num_interactions this.num_optional this.num_experiments this.num_components_in_experiment        
    member this.ToShortString() = 
        sprintf "%i, %i, %i, %i, %i, %i" this.num_components this.num_interactions this.num_optional this.num_experiments this.num_components_in_experiment (if this.valid_only then 1 else 0)
    static member Create(components, interactions, optional, experiments, comp_per_exp, valid_only) = 
        {num_components = components
        ; num_interactions = interactions
        ; num_optional = optional
        ; num_experiments = experiments
        ; num_components_in_experiment = comp_per_exp
        ; valid_only = valid_only}

//generate num unique random numbers between 0 and max with generator rnd
let genUnique (rnd:System.Random) num max = 
    if num > max then
        failwith (sprintf "Cannot generate %i unique random numbers in [0..%i)" num max)
    
    let rec permute (a: int list)= 
        match a with
        | [] -> []
        | [v] -> [v]
        | l -> 
            let N = l.Length
            let n = rnd.Next(N)
            let v = l.[n]
            let l' = 
                if n>0 then 
                    l |> Seq.take n |> List.ofSeq 
                else 
                    []                        
            let l'' = 
                if n<N then 
                    l |> Seq.skip (n+1) |> Seq.take (N-n-1) |> List.ofSeq
                else
                    []        
            [v]
            |> List.append (permute l')
            |> List.append (permute l'')        
      
    [0..max-1] 
    |> permute
    |> Seq.take num
    |> Set.ofSeq


//    Seq.fold(fun (set:Set<int>) _ ->             
//            let i = ref (rnd.Next(max))
//            while set.Contains(!i) do                       
//                i := rnd.Next(max)
//            set.Add(!i)) Set.empty [0..num]



let sampleConcreteModel (model:REIN.Problem) =
    let rnd = new System.Random();     
    let interactions' = 
        seq {
            for i in model.interactions do            
                if i.definite then
                    yield i //return all definite interactions
                else                
                    if (rnd.Next(2)>0) then  //randomly include optional interacitons
                        yield i.MkDefinite()
            }


    let species' = 
        seq {
             for s in model.species do
                
                let rc = 
                    match s.reg_conds with
                    | Some(r) -> 
                        let id = rnd.Next(r.Length) //generate a random regulation condition                                        
                        Some([r.[id]])
                    | None ->                       //all regulation conditions are allowed, so pick one
                        let id = rnd.Next(Settings.maxConditions.[model.settings.regulation])
                        Some([id])                

                yield {s with reg_conds = rc}
            }

    {model with interactions = interactions'; species = species'}






//generate a REIN model with num_genes genes, num_interactions interactions out of which num_optional are optional
let generateModel num_components num_interactions num_optional = 
    let rnd = new System.Random();    

    //generate random components
    let components = Array.init num_components (fun i -> sprintf "C%i" i)        

    let interactions = 
        Seq.fold(fun (set:Set<int*int*bool>) _ -> 
            let source = rnd.Next(num_components)
            let target = rnd.Next(num_components)
            let pos    = rnd.Next(2)>0
            let i = ref (source,target,pos)
            while set.Contains(!i) do       
                let source = rnd.Next(num_components)
                let target = rnd.Next(num_components)
                let pos    = rnd.Next(2)>0
                i := (source,target,pos)
            set.Add(!i)) Set.empty [0..num_interactions-1]      

    let optional = genUnique rnd num_optional num_interactions


    let componentsStr = components |> Seq.reduce (fun a b -> sprintf "%s; %s" a b)
    let interactionsStr = 
        if Seq.isEmpty interactions  then ""
        else
            interactions
            |> Seq.mapi(fun i (source,target,positive) -> 
                let pos = if positive then "positive" else "negative" 
                let opt = if optional.Contains(i) then "\toptional" else ""
                sprintf "%s\t%s\t%s%s" components.[source] components.[target] pos opt)
            |> Seq.reduce(fun a b -> sprintf "%s;\n%s" a b)
            |> sprintf "%s;"

    let model = sprintf "%s;\n%s" componentsStr interactionsStr        

    (model, components)


//generate a specification on the set of genes [genes] where [num_genes] of the genes are set to random initial and final values in each of [num_experiments] experiments
let generateSpec genes num_genes num_experiments =     
    let n = Array.length(genes)
    let rnd = new System.Random();

    if num_experiments = 0 then 
        ""
    else
        Array.init num_experiments (fun i ->         
                let expName = "#Exp" + i.ToString()
                let g = genUnique rnd num_genes n
        
                let initial = 
                    Seq.map(fun id ->                
                            let value = if rnd.Next(2)>0 then "1" else "0"
                            expName + "[0]." + genes.[id] + "=" + value
                        ) g                             
                    |>Seq.reduce(fun a b -> a + " and\n" + b)

                let final = 
                    Seq.map(fun id ->                
                            let value = if rnd.Next(2)>0 then "1" else "0"
                            expName + "[18]." + genes.[id] + "=" + value + " and\n" + 
                            expName + "[19]." + genes.[id] + "=" + value
                        ) g                             
                    |>Seq.reduce(fun a b -> a + " and\n" + b)
                                
                initial + ";\n" + final)
        |> Seq.reduce(fun a b -> a + ";\n\n" + b)
        |> sprintf "%s;"



//Strategy 1: sample a model, define initial conditions, find trajectories, pick final states
//Strategy 2 (future): generate a solution (or multiple solutions), look at the generated trajectories and define a spec
let generateValidSpec (M:string) num_experiments components_per_experiment=     
    
    let model = M |> REIN.ParseAST

    let rnd = new System.Random();
   
    //make an array of component names
    let components = model.species |> Seq.map(fun s -> s.name) |> Array.ofSeq     
                              
        
    let experiment_names = Array.init num_experiments (fun i -> sprintf "#Exp%i" i)

    let initial = 
        experiment_names
        |> Array.map (fun e -> 
            let exp_comps = genUnique rnd components_per_experiment components.Length

            let exp_string = 
                exp_comps
                |> Seq.map(fun s -> components.[s],rnd.Next(2)>0)
                |> Seq.map(fun (s,v) -> sprintf "%s[0].%s=%i" e s (if v then 1 else 0))
                |> Seq.reduce(fun a b -> a + " and\n" + b)                                                
            
            (exp_comps, exp_string)
            )

    //sample the input ABN to get a concrete model and add initial states spec
    let modelStr = 
        model 
        |> sampleConcreteModel     
        |> fun m -> m.ToString()        

    let initSpecStr =  
        if Seq.isEmpty initial then ""
        else
            (initial |> Seq.map snd |> Seq.reduce(fun a b -> a + ";\n\n" + b)  ) + ";"  
    
                    
    let solOpt =            
        sprintf "%s\n%s" modelStr initSpecStr //construct model with specs
        |> REIN.ParseAST
        |> Procedures.Solve            
        
    
    let spec = 
        if Array.isEmpty experiment_names then 
            ""
        else        
            match solOpt with
            | Some(sol) ->                     
                experiment_names
                |> Array.mapi(fun i e -> 
                    let finalExpState = sol.paths.[e].states.[20]
                
                    let final = 
                        initial.[i] 
                        |> fst 
                        |> Seq.map(fun s -> 
                            let c = components.[s]                    
                            (c,finalExpState.[c])
                            )        
                        |> Seq.map(fun (s,v) -> sprintf "%s[20].%s=%i" e s (if v="true" then 1 else 0))
                        |> Seq.reduce(fun a b -> a + " and\n" + b)                         
                
                    sprintf "%s and\n%s" (snd initial.[i]) final
                    )            
            | None -> failwith "Non-consistent model was generated"
            |> Seq.reduce(fun a b -> a + ";\n\n" + b)
            |> sprintf "%s;" 
    //return the specification         
    (spec, modelStr)
   



//TODO: Expand to generate perturbations
//TODO: Use an external simulation for valid problems to avoid encoding dependence
let generate (opt:genSettings) =  
    let model,components = generateModel opt.num_components opt.num_interactions opt.num_optional
    let spec = 
        if opt.valid_only then
            generateValidSpec model opt.num_components_in_experiment opt.num_experiments |> fst
        else
            generateSpec components opt.num_components_in_experiment opt.num_experiments
    
    
    "//Model\n\n" + model + "\n\n\n\n//Observations\n\n" + spec