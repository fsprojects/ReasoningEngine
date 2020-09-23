module Microsoft.Research.REIN.RegulationTables
(* Testing and debugging functions for generating tables of regulation conditions *)


module Expressions = Microsoft.Research.ReasoningEngine.Constraint
module Var = Microsoft.Research.ReasoningEngine.Var

let RegulationConditionTableUsingOptionals (reg:Settings.RegulationConditions) = 
    let set_initial name value (model:REIN.Problem) = 
        let bvalue = if value = 1 then true else false
        model.AddConstraint(REIN.Fact.Create(Expressions.Beq(Expressions.BTerm(Expressions.BVar(Var.StateVar("Traj",0,name))),Expressions.BTerm(Expressions.BConst(bvalue))),None))                

    let SetInteractionState (i:REIN.Interaction) bvalue (model:REIN.Problem)=         
        model.AddConstraint(REIN.Fact.Create(Expressions.Beq(Expressions.BTerm(Expressions.BVar(Var.SysVar(i.var))),Expressions.BTerm(Expressions.BConst(bvalue))),None))

    let interactions = 
        [| REIN.Interaction.Create("A","T",true,false)                
        ; REIN.Interaction.Create("B","T",true,false)                
        ; REIN.Interaction.Create("C","T",false,false)                
        ; REIN.Interaction.Create("D","T",false,false)|]
    //define the default model with appropriate settings (but no species or interactions yet)
    [for reg_cond in [0..Settings.maxConditions.[reg]] do
        let default_model = 
            {REIN.Problem.empty with settings = {REIN.Problem.empty.settings with regulation = reg; traj_length = 2}}
            |> fun m -> m.AddSpecies(REIN.Species.Create("T",Some([reg_cond])))
            |> fun m -> m.AddSpecies(REIN.Species.Create("A",Some([0])))
            |> fun m -> m.AddSpecies(REIN.Species.Create("B",Some([0])))
            |> fun m -> m.AddSpecies(REIN.Species.Create("C",Some([0])))
            |> fun m -> m.AddSpecies(REIN.Species.Create("D",Some([0])))
            |> fun m -> interactions |> Seq.fold(fun (ma:REIN.Problem) i -> ma.AddInteraction(i)) m

                                                                        
       //construct a model without any regulators
        let model =  default_model        
        let res_const= 
            let sol = 
                model 
                |> set_initial "T" 1 
                |> Procedures.Solve
            if sol.IsNone then failwith "No solutions found!" else
                sol.Value.paths.["Traj"].states.[1].["T"]           
       
        //construct a model with two activators only
        let model = 
            default_model
            |> SetInteractionState interactions.[0] true
            |> SetInteractionState interactions.[1] true     
            |> SetInteractionState interactions.[2] false
            |> SetInteractionState interactions.[3] false      

        let res_act = seq {
            for A in [0..1] do
                for B in [0..1] do
                    let sol = 
                        model 
                        |> set_initial "A" A 
                        |> set_initial "B" B                            
                        |> Procedures.Solve
                    if sol.IsNone then failwith "No solutions found!" else 
                        yield ((A+B), sol.Value.paths.["Traj"].states.[1].["T"])                                                                                                  
            }


        //construct a model with two repressors only
        let model = 
            default_model
            |> SetInteractionState interactions.[0] false
            |> SetInteractionState interactions.[1] false    
            |> SetInteractionState interactions.[2] true
            |> SetInteractionState interactions.[3] true


        let res_rep = seq {
            for C in [0..1] do
                for D in [0..1] do
                    let sol = 
                        model 
                        |> set_initial "C" C 
                        |> set_initial "D" D                          
                        |> Procedures.Solve  
                    if sol.IsNone then failwith "No solutions found!" else 
                        yield ((C+D), sol.Value.paths.["Traj"].states.[1].["T"])                                 
            }


        //construct a full model 
        let model = 
            default_model
            |> SetInteractionState interactions.[0] true
            |> SetInteractionState interactions.[1] true    
            |> SetInteractionState interactions.[2] true
            |> SetInteractionState interactions.[3] true
        
        let res_full = seq {
            for A in [0..1] do
                for B in [0..1] do
                    for C in [0..1] do
                        for D in [0..1] do
                            let sol = 
                                model 
                                |> set_initial "A" A
                                |> set_initial "B" B
                                |> set_initial "C" C
                                |> set_initial "D" D                                                                                                             
                                |> Procedures.Solve  
                            if sol.IsNone then failwith "No solutions found!" else 
                                yield ((A+B), (C+D), sol.Value.paths.["Traj"].states.[1].["T"])                                                                                                                                                                              
            }

        //print the results
        yield List.concat([ [res_const];
                            [                            
                            for i in [0..2] do
                                let a = Seq.filter(fun (n, _) -> n=i) res_act
                                if Seq.length (Seq.distinct a) <> 1 then 
                                    failwith "Inconsistency"
                                yield ((Seq.head a) |> snd)
                            ];
                            [for i in [0..2] do
                                let a = Seq.filter(fun (n, _) -> n=i) res_rep
                                if Seq.length (Seq.distinct a) <> 1 then 
                                    failwith "Inconsistency"
                                yield ((Seq.head a) |> snd)
                            ];
                            [for i in [0..2] do
                                for j in [0..2] do
                                let a = Seq.filter(fun (n,m,_) -> n=j && m=i) res_full
                                if Seq.length (Seq.distinct a) <> 1 then 
                                    failwith "Inconsistency"
                                else 
                                    let (_, _, v) = (Seq.head a)
                                    yield v
                            ]
                        ])        
    ]



let RegulationConditionTable (reg:Settings.RegulationConditions) = 

    let set_initial name value (model:REIN.Problem) = 
        let bvalue = if value = 1 then true else false
        model.AddConstraint(REIN.Fact.Create(Expressions.Beq(Expressions.BTerm(Expressions.BVar(Var.StateVar("Traj",0,name))),Expressions.BTerm(Expressions.BConst(bvalue))),None))                

    //define the default model with appropriate settings (but no species or interactions yet)
    let default_model = {REIN.Problem.empty with settings = {REIN.Problem.empty.settings with regulation = reg; traj_length = 2}}
    
    [for reg_cond in [0..Settings.maxConditions.[reg]] do
       //construct a model without any regulators
        let model = default_model        
        let model = model.AddSpecies(REIN.Species.Create("T",Some([reg_cond]),false,false))                 
        let res_const= 
            let sol = 
                model 
                |> set_initial "T" 1 
                |> Procedures.Solve   
            if sol.IsNone then failwith "No solutions found!" else
                sol.Value.paths.["Traj"].states.[1].["T"]           
       
        //construct a model with two activators only
        let model = default_model

        let model = model.AddSpecies(REIN.Species.Create("A",Some([0]),false,false))
        let model = model.AddSpecies(REIN.Species.Create("B",Some([0]),false,false))
        let model = model.AddSpecies(REIN.Species.Create("T",Some([reg_cond]),false,false))
         
        let model = model.AddInteraction(REIN.Interaction.Create("A","T",true,true))
        let model = model.AddInteraction(REIN.Interaction.Create("B","T",true,true))

        let res_act = seq {
            for A in [0..1] do
                for B in [0..1] do
                    let sol = 
                        model 
                        |> set_initial "A" A 
                        |> set_initial "B" B                            
                        |> Procedures.Solve
                    if sol.IsNone then failwith "No solutions found!" else 
                        yield ((A+B), sol.Value.paths.["Traj"].states.[1].["T"])                                                                                                  
            }


        //construct a model with two repressors only
        let model = default_model
        let model = model.AddSpecies(REIN.Species.Create("C",Some([0]),false,false))
        let model = model.AddSpecies(REIN.Species.Create("D",Some([0]),false,false))
        let model = model.AddSpecies(REIN.Species.Create("T",Some([reg_cond]),false,false))
         
        let model = model.AddInteraction(REIN.Interaction.Create("C","T",false,true))
        let model = model.AddInteraction(REIN.Interaction.Create("D","T",false,true))

        let res_rep = seq {
            for C in [0..1] do
                for D in [0..1] do
                    let sol = 
                        model 
                        |> set_initial "C" C 
                        |> set_initial "D" D                          
                        |> Procedures.Solve   
                    if sol.IsNone then failwith "No solutions found!" else 
                        yield ((C+D), sol.Value.paths.["Traj"].states.[1].["T"])                                 
            }


        //construct a full model 
        let model = default_model
        let model = model.AddSpecies(REIN.Species.Create("A",Some([0]),false,false))
        let model = model.AddSpecies(REIN.Species.Create("B",Some([0]),false,false))
        let model = model.AddSpecies(REIN.Species.Create("C",Some([0]),false,false))
        let model = model.AddSpecies(REIN.Species.Create("D",Some([0]),false,false))
        let model = model.AddSpecies(REIN.Species.Create("T",Some([reg_cond]),false,false))
         
        let model = model.AddInteraction(REIN.Interaction.Create("A","T",true,true))
        let model = model.AddInteraction(REIN.Interaction.Create("B","T",true,true))
        let model = model.AddInteraction(REIN.Interaction.Create("C","T",false,true))
        let model = model.AddInteraction(REIN.Interaction.Create("D","T",false,true))

        let res_full = seq {
            for A in [0..1] do
                for B in [0..1] do
                    for C in [0..1] do
                        for D in [0..1] do
                            let sol = 
                                model 
                                |> set_initial "A" A
                                |> set_initial "B" B
                                |> set_initial "C" C
                                |> set_initial "D" D                                                                                                             
                                |> Procedures.Solve   
                            if sol.IsNone then failwith "No solutions found!" else 
                                yield ((A+B), (C+D), sol.Value.paths.["Traj"].states.[1].["T"])                                                                                                                                                                              
            }

        //print the results
        yield List.concat([ [res_const];
                            [                            
                            for i in [0..2] do
                                let a = Seq.filter(fun (n, _) -> n=i) res_act
                                if Seq.length (Seq.distinct a) <> 1 then 
                                    failwith "Inconsistency"
                                yield ((Seq.head a) |> snd)
                            ];
                            [for i in [0..2] do
                                let a = Seq.filter(fun (n, _) -> n=i) res_rep
                                if Seq.length (Seq.distinct a) <> 1 then 
                                    failwith "Inconsistency"
                                yield ((Seq.head a) |> snd)
                            ];
                            [for i in [0..2] do
                                for j in [0..2] do
                                let a = Seq.filter(fun (n,m,_) -> n=j && m=i) res_full
                                if Seq.length (Seq.distinct a) <> 1 then 
                                    failwith "Inconsistency"
                                else 
                                    let (_, _, v) = (Seq.head a)
                                    yield v
                            ]
                        ])        
    ]