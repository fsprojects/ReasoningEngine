module  Microsoft.Research.REIN.Regulation

open Microsoft.Research.ReasoningEngine.Constraint
let GenerateDefaultRegCond (mem: Memoization.Memoization) (t:Settings.RegulationConditions) =        
    match t with
    | Settings.Legacy ->     
            let SomeButNotAllActivatorsPresent s = And(mem.activator_absent.[s], mem.activator_present.[s]) // Not all activators present, but at least one            
            let AllActivatorsPresent s = And(Not(mem.activator_absent.[s]), mem.activator_present.[s]); // All of the activators are present                
            let SomeButNotAllRepressorsPresent s = And(mem.repressor_absent.[s], mem.repressor_present.[s]);    // Not all repressors present, but at least one
            let NoRepressorsPresent s = And(mem.repressor_absent.[s], Not(mem.repressor_present.[s]));   // None of the repressors are present (NB. assumes that the gene is repressible)
            let AllRepressorsPresent s = And(Not(mem.repressor_absent.[s]), mem.repressor_present.[s]); // All repressors present
            [|
               fun s ->  And(mem.not_repressible.[s], AllActivatorsPresent s);
               fun s ->  And(mem.not_repressible.[s], Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s));
               fun s ->  Or(And(NoRepressorsPresent s, Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s)), And(mem.not_repressible.[s], AllActivatorsPresent s));
               fun s ->  And(Or(NoRepressorsPresent s, mem.not_repressible.[s]), Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s));
               fun s ->  And(AllActivatorsPresent s, Not(AllRepressorsPresent s));
               fun s ->  Or(And(mem.not_repressible.[s], SomeButNotAllActivatorsPresent s), And(AllActivatorsPresent s, Not(AllRepressorsPresent s)));
               fun s ->  Or(And(SomeButNotAllActivatorsPresent s, Or(NoRepressorsPresent s, SomeButNotAllRepressorsPresent s)), And(AllActivatorsPresent s, Not(AllRepressorsPresent s)));
               fun s ->  And(Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s), Not(AllRepressorsPresent s));
               fun s ->  AllActivatorsPresent s;
               fun s ->  Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, mem.not_repressible.[s]));
               fun s ->  Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, NoRepressorsPresent s));
               fun s ->  Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Or(mem.not_repressible.[s], NoRepressorsPresent s)));
               fun s ->  Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Or(NoRepressorsPresent s, SomeButNotAllRepressorsPresent s)));
               fun s ->  Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Not(AllRepressorsPresent s)));
               fun s ->  Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Not(mem.not_repressible.[s])));
               fun s ->  Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s);
               fun s ->  And(SomeButNotAllRepressorsPresent s, mem.not_inducible.[s]);
               fun s ->  And(NoRepressorsPresent s, mem.not_inducible.[s]);
                // Not possible to include the thresholding regulation condition if we use this style of definition
            |]  
    | Settings.NoThresholds ->           
            [|
               fun s -> And(mem.all_activators.[s], mem.no_repressors.[s]);
               fun s -> And(Not(mem.no_activators.[s]), mem.no_repressors.[s])
               fun s -> And(mem.all_activators.[s], (Not (mem.all_repressors.[s])));
               fun s -> Or (And(mem.no_repressors.[s], (Not (mem.no_activators.[s]))), And (Not (mem.all_repressors.[s]), (mem.all_activators.[s])));
               fun s -> mem.all_activators.[s];
               fun s -> Or (mem.all_activators.[s], (And (mem.no_repressors.[s], (Not (mem.no_activators.[s])))));
               fun s -> And (Not (mem.no_activators.[s]), (Not (mem.all_repressors.[s])));
               fun s -> Or (And (Not (mem.no_activators.[s]), Not (mem.all_repressors.[s])), mem.all_activators.[s]);
               fun s -> Not (mem.no_activators.[s]);
               fun s -> mem.no_repressors.[s];
               fun s -> Or (mem.no_repressors.[s], And (Not (mem.all_repressors.[s]), mem.all_activators.[s]));
               fun s -> Or (mem.no_repressors.[s], And (Not (mem.no_activators.[s]),  Not (mem.all_repressors.[s])));
               fun s -> Not (mem.all_repressors.[s]);                                                                                                                                                                            
               fun s -> Or (mem.no_repressors.[s], mem.all_activators.[s]);
               fun s -> Or (Or (mem.no_repressors.[s], mem.all_activators.[s]),  And (Not (mem.all_repressors.[s]), Not (mem.no_activators.[s])));
               fun s -> Or (Not (mem.all_repressors.[s]), mem.all_activators.[s]); 
               fun s -> Or (mem.no_repressors.[s], Not (mem.no_activators.[s]));                
               fun s -> Or (Not (mem.all_repressors.[s]), Not (mem.no_activators.[s]));
            |]
            |> Array.map(fun base_regulation -> 
                let condition1 s = Imp(And (Not (mem.not_inducible.[s]), (mem.not_repressible.[s])), Not (mem.no_activators.[s]))
                let condition2 s = And(And (Not (mem.not_repressible.[s]), (mem.not_inducible.[s])), mem.no_repressors.[s])
                fun s -> Or(And (base_regulation s, condition1 s), condition2 s))  
     | Settings.Default ->        
            let reg_conds =               
                [|
                    fun s -> And(mem.all_activators.[s], mem.no_repressors.[s]);
                    fun s -> And(Not(mem.no_activators.[s]), mem.no_repressors.[s])
                    fun s -> And(mem.all_activators.[s], (Not (mem.all_repressors.[s])));
                    fun s -> Or (And(mem.no_repressors.[s], (Not (mem.no_activators.[s]))), And (Not (mem.all_repressors.[s]), (mem.all_activators.[s])));
                    fun s -> mem.all_activators.[s];
                    fun s -> Or (mem.all_activators.[s], (And (mem.no_repressors.[s], (Not (mem.no_activators.[s])))));
                    fun s -> And (Not (mem.no_activators.[s]), (Not (mem.all_repressors.[s])));
                    fun s -> Or (And (Not (mem.no_activators.[s]), Not (mem.all_repressors.[s])), mem.all_activators.[s]);
                    fun s -> Not (mem.no_activators.[s]);
                    fun s -> mem.no_repressors.[s];
                    fun s -> Or (mem.no_repressors.[s], And (Not (mem.all_repressors.[s]), mem.all_activators.[s]));
                    fun s -> Or (mem.no_repressors.[s], And (Not (mem.no_activators.[s]),  Not (mem.all_repressors.[s])));
                    fun s -> Not (mem.all_repressors.[s]);                                                                                                                                                                            
                    fun s -> Or (mem.no_repressors.[s], mem.all_activators.[s]);
                    fun s -> Or (Or (mem.no_repressors.[s], mem.all_activators.[s]),  And (Not (mem.all_repressors.[s]), Not (mem.no_activators.[s])));
                    fun s -> Or (Not (mem.all_repressors.[s]), mem.all_activators.[s]); 
                    fun s -> Or (mem.no_repressors.[s], Not (mem.no_activators.[s]));                
                    fun s -> Or (Not (mem.all_repressors.[s]), Not (mem.no_activators.[s]));
                |]
                |> Array.map(fun base_regulation -> 
                    let condition1 s = Imp(And (Not (mem.not_inducible.[s]), (mem.not_repressible.[s])), Not (mem.no_activators.[s]))
                    let condition2 s = And(And (Not (mem.not_repressible.[s]), (mem.not_inducible.[s])), mem.no_repressors.[s])
                    fun s -> Or(And (base_regulation s, condition1 s), condition2 s))  

            //for threshold regulation conditions
            let AA s = mem.num_available_activators.[s] //available activators            
            let AR s = mem.num_available_repressors.[s] //available repressors                      
            let more_activators_than_repressors s = BTerm(BComp(Gt(AA s,AR s))) //the number of available activators is greater than the number of available repressor
            let equal_activators_and_repressors s = BTerm(BComp(Eq(AA s,AR s))) //the number of available activators is equal to the number of available repressor
            let available s = BTerm(BVar(Microsoft.Research.ReasoningEngine.Var.AbsStateVar(-1,s))) //the state of the species in the previous time step
            let threshold =             
                [|fun s ->  Or(more_activators_than_repressors s, And(equal_activators_and_repressors s, available s))  // The thresholding rule  
                 ;fun s ->  more_activators_than_repressors s|] // The slightly different threshold rule for nodes with self-degration in (Li et al. PNAS, 2004). These nodes will degrade if a         

            //return the updated regulation conditions with the threshold rules appended at the end
            Array.append reg_conds threshold   
    | _ -> failwith "Regulation conditions definition not implemented yet"              