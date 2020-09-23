module  Microsoft.Research.REIN.Memoization

open Microsoft.Research.ReasoningEngine.Constraint
open Microsoft.Research.ReasoningEngine.Var

type Memoization = 
    { optional_interactions:REIN.Interaction list
    ; definite_interactions:REIN.Interaction list
    ; num_opt_interactions: int
    ; optional_positive_interactions: Map<string,seq<REIN.Interaction>>
    ; optional_negative_interactions: Map<string,seq<REIN.Interaction>>
    ; definite_positive_interactions: Map<string,seq<REIN.Interaction>>
    ; definite_negative_interactions: Map<string,seq<REIN.Interaction>>
    ; all_activators:Map<string, BExpr>
    ; no_activators:Map<string, BExpr>
    ; all_repressors:Map<string, BExpr>
    ; no_repressors:Map<string, BExpr>
    ; not_inducible:Map<string, BExpr>
    ; not_repressible:Map<string, BExpr>
    ; activator_present : Map<string, BExpr>
    ; activator_absent  : Map<string, BExpr>
    ; repressor_present : Map<string, BExpr>
    ; repressor_absent  : Map<string, BExpr>
    ; num_available_activators:Map<string, NExpr>
    ; num_available_repressors:Map<string, NExpr>
    }


let Memoize (problem:REIN.Problem) =   
    //split the interactions into optional and definite
    let optional_interactions,definite_interactions = 
        problem.interactions 
        |> Seq.fold(fun (opt,def) i -> if i.definite then (opt, i::def) else (i::opt,def) ) (List.empty, List.empty)                  
            
    //count the optional interactions
    let num_opt_interactions = optional_interactions |> List.length    

    let optional_positive_interactions = 
        problem.species
        |> Seq.map(fun s -> s.name, optional_interactions |> Seq.filter(fun i -> i.target=s.name && i.positive))            
        |> Map.ofSeq

    let optional_negative_interactions = 
        problem.species
        |> Seq.map(fun s -> s.name, optional_interactions |> Seq.filter(fun i -> i.target=s.name && not(i.positive)))        
        |> Map.ofSeq

    let definite_positive_interactions = 
        problem.species
        |> Seq.map(fun s -> s.name, definite_interactions |> Seq.filter(fun i -> i.target=s.name && i.positive))        
        |> Map.ofSeq

    let definite_negative_interactions = 
        problem.species
        |> Seq.map(fun s -> s.name, definite_interactions |> Seq.filter(fun i -> i.target=s.name && not(i.positive)))
        |> Map.ofSeq

    let RegulatorState s = BTerm(BVar(AbsStateVar(-1,s)))
    let InteractionState (i:REIN.Interaction) = BTerm(BVar(SysVar(i.var)))
    let False = BTerm(BConst(false))
    let True = BTerm(BConst(true))


    let not_inducible =         
        problem.species
        |> Seq.map(fun s ->       
            let exp = 
                if not (Seq.isEmpty definite_positive_interactions.[s.name]) then False
                else
                    if Seq.isEmpty optional_positive_interactions.[s.name] then True else
                        optional_positive_interactions.[s.name]
                        |> Seq.map InteractionState
                        |> LOr
                        |> Not                          
            s.name,exp
            )
        |> Map.ofSeq

    let not_repressible =         
        problem.species
        |> Seq.map(fun s ->       
            let exp = 
                if not (Seq.isEmpty definite_negative_interactions.[s.name]) then False
                else
                    if Seq.isEmpty optional_negative_interactions.[s.name] then True else
                        optional_negative_interactions.[s.name]
                        |> Seq.map InteractionState
                        |> LOr
                        |> Not                          
            s.name,exp
            )
        |> Map.ofSeq

    let all_activators = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name            
            let definite = 
                definite_positive_interactions.[s]
                |> Seq.map(fun i -> RegulatorState i.source)
                |> Array.ofSeq            
            let optional = 
                optional_positive_interactions.[s]
                |> Seq.map(fun i -> Imp(InteractionState i, RegulatorState i.source))
                |> Array.ofSeq            
            let all = 
                definite
                |> Array.append optional
                |> Array.append [|Not not_inducible.[s]|]
            s, if Array.length all = 0 then False else LAnd(all)
            ) 
        |> Map.ofSeq

    let all_repressors = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name            
            let definite = 
                definite_negative_interactions.[s] 
                |> Seq.map(fun i -> RegulatorState i.source)
                |> Array.ofSeq            
            let optional = 
                optional_negative_interactions.[s]
                |> Seq.map(fun i -> Imp(InteractionState i, RegulatorState i.source))
                |> Array.ofSeq            
            let all = 
                definite
                |> Array.append optional
                |> Array.append [|Not not_repressible.[s]|]
            s, if Array.length all = 0 then False else LAnd(all)
            ) 
        |> Map.ofSeq

    let no_activators = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name            
            let definite = 
                definite_positive_interactions.[s]
                |> Seq.map(fun i -> Not(RegulatorState i.source))
                |> Array.ofSeq            
            let optional = 
                optional_positive_interactions.[s]
                |> Seq.map(fun i -> Imp(InteractionState i, Not(RegulatorState i.source)))
                |> Array.ofSeq            
            let all = Array.append(definite) optional
            s, if Array.length all = 0 then True else LAnd(all)
            ) 
        |> Map.ofSeq

    let no_repressors = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name            
            let definite = 
                definite_negative_interactions.[s] 
                |> Seq.map(fun i -> Not(RegulatorState i.source))
                |> Array.ofSeq            
            let optional = 
                optional_negative_interactions.[s]
                |> Seq.map(fun i -> Imp(InteractionState i, Not(RegulatorState i.source)))
                |> Array.ofSeq            
            let all = Array.append(definite) optional
            s, if Array.length all = 0 then True else LAnd(all)
            ) 
        |> Map.ofSeq
   

    let activator_present = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name                            
            let definite = 
                definite_positive_interactions.[s]
                |> Seq.map(fun i -> RegulatorState i.source)
                |> Array.ofSeq 
            let optional = 
                optional_positive_interactions.[s]
                |> Seq.map(fun i -> And(InteractionState i, RegulatorState i.source))
                |> Array.ofSeq
            let all = Array.append(definite) optional
            s, if Array.length all = 0 then False else LOr(all)                      
        )
        |> Map.ofSeq

    let activator_absent = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name                            
            let definite = 
                definite_positive_interactions.[s]
                |> Seq.map(fun i -> Not(RegulatorState i.source))
                |> Array.ofSeq 
            let optional = 
                optional_positive_interactions.[s]
                |> Seq.map(fun i -> And(InteractionState i, Not(RegulatorState i.source)))
                |> Array.ofSeq
            let all = Array.append(definite) optional
            s, if Array.length all = 0 then False else LOr(all)                      
        )
        |> Map.ofSeq
      

    let repressor_present = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name                            
            let definite = 
                definite_negative_interactions.[s]
                |> Seq.map(fun i -> RegulatorState i.source)
                |> Array.ofSeq 
            let optional = 
                optional_negative_interactions.[s]
                |> Seq.map(fun i -> And(InteractionState i, RegulatorState i.source))
                |> Array.ofSeq
            let all = Array.append(definite) optional
            s, if Array.length all = 0 then False else LOr(all)                      
        )
        |> Map.ofSeq

    let repressor_absent = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name                            
            let definite = 
                definite_negative_interactions.[s]
                |> Seq.map(fun i -> Not(RegulatorState i.source))
                |> Array.ofSeq 
            let optional = 
                optional_negative_interactions.[s]
                |> Seq.map(fun i -> And(InteractionState i, Not(RegulatorState i.source)))
                |> Array.ofSeq
            let all = Array.append(definite) optional
            s, if Array.length all = 0 then False else LOr(all)                      
        )
        |> Map.ofSeq



    let num_available_activators = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name            
            let definite = 
                definite_positive_interactions.[s]
                |> Seq.map(fun i -> RegulatorState i.source)
                |> Array.ofSeq 
            let optional = 
                optional_positive_interactions.[s]
                |> Seq.map(fun i -> And(InteractionState i, RegulatorState i.source))
                |> Array.ofSeq
            let all = Array.append(definite) optional
            s, NTerm(if Array.length all = 0 then NConst(0) else Card(all)))
        |> Map.ofSeq

    let num_available_repressors = 
        problem.species
        |> Seq.map(fun species -> 
            let s = species.name            
            let definite = 
                definite_negative_interactions.[s]
                |> Seq.map(fun i -> RegulatorState i.source)
                |> Array.ofSeq 
            let optional = 
                optional_negative_interactions.[s]
                |> Seq.map(fun i -> And(InteractionState i, RegulatorState i.source))
                |> Array.ofSeq
            let all = Array.append(definite) optional
            s, NTerm(if Array.length all = 0 then NConst(0) else Card(all)))
        |> Map.ofSeq



    { optional_interactions = optional_interactions
    ; definite_interactions = definite_interactions
    ; num_opt_interactions = num_opt_interactions
    ; optional_positive_interactions = optional_positive_interactions
    ; optional_negative_interactions = optional_negative_interactions
    ; definite_positive_interactions = definite_positive_interactions
    ; definite_negative_interactions = definite_negative_interactions
    ; all_activators = all_activators
    ; no_activators  = no_activators
    ; all_repressors = all_repressors
    ; no_repressors  = no_repressors
    ; not_inducible = not_inducible
    ; not_repressible = not_repressible
    ; activator_present = activator_present
    ; activator_absent  = activator_absent 
    ; repressor_present = repressor_present
    ; repressor_absent  = repressor_absent
    ; num_available_activators = num_available_activators
    ; num_available_repressors = num_available_repressors
    }