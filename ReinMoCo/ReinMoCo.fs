namespace Microsoft.Research.REINMoCo

open Parser
open Microsoft.Research.ReasoningEngine.Constraint
//open System.Runtime.Remoting.Contexts

module REIN = Microsoft.Research.REIN.REIN        
module Settings = Microsoft.Research.REIN.Settings

type Motif = 
    { components    : seq<string>
    ; interactions  : seq<REIN.Interaction>
    ; name          : string
    }
    override this.ToString() = 
        this.interactions
        |> Seq.map(fun i -> sprintf "\t%s" (i.ToModelString()))
        |> String.concat ",\r\n"
        |> sprintf "%s:={\n%s\n\t}" this.name 

type Problem =
    { species         : seq<REIN.Species>
    ; interactions    : seq<REIN.Interaction>    
    ; predicates      : Map<string,BExpr>   
    ; constraints     : seq<REIN.Fact>  
    ; settings        : Settings.Settings
    ; motifs          : Map<string, Motif>
    } 

    static member empty = 
        { species      = Seq.empty
        ; interactions = Seq.empty
        ; predicates   = Map.empty
        ; constraints  = Seq.empty
        ; settings     = Settings.Settings.Default
        ; motifs       = Map.empty
        }           
    
    member this.AddInteraction(i) = {this with interactions = Seq.append(this.interactions) [i]} 

    member this.AddSpecies(s) = {this with species = Seq.append(this.species) [s]} 

    member this.MkAllInteractionsPossible()  = 
        {this with interactions = this.interactions |> Seq.map(fun i -> i.MkPossible())}
    
    member this.MkAllInteractionsDefinite()  = 
        {this with interactions = this.interactions |> Seq.map(fun i -> i.MkDefinite())}
    
    member this.MkPossible (source, target) = 
        {this with interactions = this.interactions |> Seq.map(fun i -> if i.source=source && i.target=target then i.MkPossible() else i)}
    
    member this.MkDefinite (source, target) = 
        {this with interactions = this.interactions |> Seq.map(fun i -> if i.source=source && i.target=target then i.MkDefinite() else i)}    
    
    member this.RemoveInteractions (source,target) = 
        {this with interactions = this.interactions |> Seq.filter(fun i -> not (i.source=source && i.target=target))}
    
    member this.RemoveInteractionsFrom source = 
        {this with interactions = this.interactions |> Seq.filter(fun i -> not (i.source=source))}
    
    member this.RemoveInteractionsTo target = 
        {this with interactions = this.interactions |> Seq.filter(fun i -> not (i.target=target))}    
    
    member this.AddAllInteractions() = 
        let interactions = 
            this.interactions
            |> Seq.map(fun i -> i.source, i.target, i.positive)
            |> Set.ofSeq        
        let new_interactions = 
            seq {
                for s in this.species do
                    for s' in this.species do
                        if not (interactions.Contains(s.name, s'.name, true)) then 
                            yield REIN.Interaction.Create(s.name,s'.name,true,false)
                        if not (interactions.Contains(s.name, s'.name, false)) then 
                            yield REIN.Interaction.Create(s.name,s'.name,false,false)
                }               
        {this with interactions = Seq.append this.interactions new_interactions}
        
    static member InitMotifPredicates (p:Problem) = 
        let motif_predicates = p.motifs |> Map.toArray |> Array.map fst |> Set.ofSeq        
        let init_motif_preds (e:BTerm) = 
            match e with 
            | Predicate p -> 
                if motif_predicates.Contains p.Name then                
                    BTerm (Predicate (p.Init "#MotifPath" 0))
                else
                    BTerm (Predicate p)
            | _ -> BTerm e
        {p with constraints = p.constraints |> Seq.map(fun f -> {f with expr = f.expr.map init_motif_preds})}


    member this.AddConstraint(f) = {this with constraints = Seq.append(this.constraints) [f]} 
    
    member this.AddPredicate(n,e) = {this with predicates = this.predicates.Add(n,e)} 
    
    member this.InlinePredicates() = {this with constraints = Seq.map(fun f -> {f with expr = f.expr.Inline this.predicates}) this.constraints}        

    override this.ToString() =    
        let settings = @"//Settings" + "\r\n" + this.settings.ToString() + "\r\n"          

        let species = 
            if Seq.isEmpty this.species then ""
            else
                this.species
                |> Seq.map (fun s -> s.ToString())
                |> String.concat ";\r\n"
                |> sprintf "//Species\r\n%s;"                                                            
        
        let interactions = 
            if Seq.isEmpty this.interactions then ""
            else
                this.interactions
                |> Seq.map (fun p -> p.ToModelString())
                |> String.concat ";\r\n"
                |> sprintf "//Interactions\r\n%s;"                                                                            
                    
        let predicates =             
            if not (Seq.isEmpty this.predicates) then
                @"//Predicates" + "\r\n" + 
                Seq.fold(fun acc (p,e) -> acc + p + ":= {" + e.ToString().Replace("p.","").Replace("p[k].","") + "};\r\n") "" (this.predicates |> Map.toSeq) + "\r\n\r\n" 
            else ""
        
        let constraints = 
            if not (Seq.isEmpty this.constraints) then
                @"//Constraints" + "\r\n" + 
                Seq.fold(fun acc p -> acc + p.ToString() + ";\r\n") "" this.constraints + "\r\n\r\n" 
            else "" 
        
        let motifs = 
            if not (Map.isEmpty this.motifs) then
                @"//Motifs" + "\r\n" + 
                Map.fold(fun acc _ p -> acc + p.ToString() + ";\r\n") "" this.motifs + "\r\n\r\n" 
            else "" 

        sprintf "%s\n%s\n%s\n%s\n%s\n%s" settings species interactions predicates motifs constraints







    static member MotifToConstraints (p:Problem) (motif:string) = 
        let m = p.motifs.[motif]

        let InteractionPresent (i:REIN.Interaction) v =  
            let value = v |> BConst |> BTerm
            let var = i.var |> Microsoft.Research.ReasoningEngine.Var.SysVar |> BVar |> BTerm                    
            Beq(var, value)

        let abn_components = p.species |> Seq.map(fun s -> s.name) |> Set.ofSeq
        let motif_definite = 
            m.interactions 
            |> Seq.filter (fun i -> i.source <> "Context" && i.target <> "Context")
            |> Seq.filter (fun i -> i.definite)            

        let abn_definite = p.interactions |> Seq.filter (fun i -> i.definite)            
        let abn_possible = p.interactions |> Seq.filter (fun i -> not i.definite)            
                        
        //an assignment maps motif components to ABN components

        let InteractionsMatch (rev_map:Map<string,string>) (others:Set<string>) (abn_i:REIN.Interaction) (motif_i:REIN.Interaction) = 
            let componentsMatch s s' = 
                (others.Contains s && s' = "Context") || (rev_map.ContainsKey s && rev_map.[s] = s')
            (componentsMatch abn_i.source motif_i.source) && 
            (componentsMatch abn_i.target motif_i.target) && 
            (abn_i.positive = motif_i.positive)

        let valid_assignment (a:Map<string,string>) =
            let rev_map = a |> Map.toArray |> Array.map(fun (a,b) -> (b,a) ) |> Map.ofArray
            let abn_components_in_motif = a |> Map.toSeq |> Seq.map snd |> Set.ofSeq
            let others = abn_components - abn_components_in_motif
            let isMatch = InteractionsMatch rev_map others
                

            let abn_motif_definite = abn_definite |> Seq.filter(fun i -> abn_components_in_motif.Contains i.source || abn_components_in_motif.Contains i.target )
            let abn_motif_possible = abn_possible |> Seq.filter(fun i -> abn_components_in_motif.Contains i.source || abn_components_in_motif.Contains i.target )


            //1) Every definite edge of the motif maps to an edge of the ABN                
            let clause1 = 
                motif_definite
                |> Seq.forall(fun i' -> p.interactions |> Seq.exists (fun i -> isMatch i i'))
                        
            //2) Every definite edge of the ABN (involving a component of the motif) maps to an edge of the motif
            let clause2 = 
                abn_motif_definite              
                |> Seq.forall(fun i -> m.interactions |> Seq.exists (fun i' -> isMatch i i'))
                        

            if clause1 && clause2 then 
                abn_motif_possible
                |> Seq.choose(fun i -> 
                    //assume only one interaction (e.g either possible or definite) between two components
                    //if it matches a definite interaction: force to be included
                    //if it does not match an interaction: force to not be included
                    //if it matches a possible interaction: no constraint
                    match m.interactions |> Seq.tryFind (fun i' -> isMatch i i') with
                    | Some i' -> 
                        if i'.definite then                            
                            Some (InteractionPresent i true)                           
                        else
                            None
                    | None ->                         
                        Some (InteractionPresent i false)                                                    
                    )                    
                |> fun x -> 
                    if Seq.isEmpty x then Some (BExpr.BTerm (BTerm.BConst true)) else
                    x |> LAnd |> Some                
            else
                None
            
        //signature: #in_pos, #in_neg, #out_pos, #out_neg
        //let GetNodeSignature (I:seq<REIN.Interaction>) name = 
        //    let out_pos = I |> Seq.filter(fun i -> i.source = name && i.positive)     |> Seq.length
        //    let out_neg = I |> Seq.filter(fun i -> i.source = name && not i.positive) |> Seq.length
        //    let in_pos = I |> Seq.filter(fun i -> i.target = name && i.positive)      |> Seq.length
        //    let in_neg = I |> Seq.filter(fun i -> i.target = name && not i.positive)  |> Seq.length
        //    (in_pos, in_neg, out_pos, out_neg)


        //let abn_signature = GetNodeSignature p.interactions
        //let motif_signature = GetNodeSignature m.interactions

        //let abn_signatures = 
        //    abn_components
        //    |> Seq.map(fun s -> s, abn_signature s)
        //    |> Seq.groupBy snd
        //    |> Seq.map(fun (S, L) -> S, L |> Seq.map fst)
        //    |> Map.ofSeq


        //possible matches
        let search_tree = 
            m.components
            |> Seq.filter(fun motif_component ->motif_component<>"Context")
            |> Seq.fold(fun acc motif_component -> 
                match acc with 
                | Some (map:Map<string,string>, matches) -> 
                    if abn_components.Contains motif_component then 
                        Some (map.Add(motif_component, motif_component), matches)
                    else
                        Some (map, (motif_component,(abn_components|>Set.toSeq))::matches)
                        //let ms = motif_signature motif_component
                        //if not (abn_signatures.ContainsKey ms) then 
                        //    None
                        //else
                        //    let S = abn_signatures.[ms]
                        //    if (Seq.length S) = 1 then                                     
                        //        Some (map.Add(motif_component, S |> Seq.head), matches)
                        //    else
                        //        Some (map, (motif_component,S)::matches)
                | None -> None
                ) (Some (Map.empty, List.empty))

        match search_tree with
        | Some (map, pairs) -> 
            let rec instantiate (M:Map<string,string>) (P:(string * seq<string>) list) = 
                if Seq.isEmpty P then 
                    [valid_assignment M]
                else
                    let motif_component, cp = P.Head                                                                   
                    let used = M |> Map.toArray |> Array.map snd |> Set.ofSeq
                    let cp' = 
                        cp
                        |> List.ofSeq
                        |> List.filter(fun abn_component -> not (used.Contains abn_component))                        

                    if List.isEmpty cp' then 
                        [None]
                    else
                        cp'
                        |> List.map(fun abn_component -> 
                            let M' = M.Add(motif_component, abn_component)
                            instantiate M' P.Tail)     
                        |> List.concat
                        
            //the motif appears somewhere
            instantiate map pairs
            |> Seq.choose id            
            |> fun x -> 
                if Seq.isEmpty x then 
                    BExpr.BTerm (BTerm.BConst false)
                else
                    LOr x
                
        | None -> BExpr.BTerm (BTerm.BConst false)  //no solutions possible
        

    static member GenMotifConstraints (p:Problem) =                  
        let motif_predicates = p.motifs |> Map.map(fun motif _->  Problem.MotifToConstraints p motif)                                                                
        let all_predicates = 
            Array.append (p.predicates |> Map.toArray) (motif_predicates  |> Map.toArray) |> Map.ofArray                
        {p with predicates = all_predicates}


    static member AstToStruct spec = 
            spec
            |> Seq.fold(fun (rein_ast:Problem) x -> 
                match x with          
                    | Interaction (a,b,c,d) -> (a,b,c,d) |> REIN.Interaction.Create |> rein_ast.AddInteraction
                    
                    | Assert(e,description) ->           
                        let fact = {REIN.Fact.expr = e; REIN.Fact.info = description} in
                        {rein_ast with constraints = Seq.append(rein_ast.constraints) [fact]}
                    
                    | Assign(n,e) -> {rein_ast with predicates = rein_ast.predicates.Add(n,e)}          
                    
                    | Species(a,b,c,d,e) -> (a,b,c,d,e) |> REIN.Species.Create |> rein_ast.AddSpecies
                    
                    | Directive(d) -> 
                        match d with
                        | Uniqueness(u) ->  {rein_ast with settings = {rein_ast.settings with uniqueness = u}} //this option is currently not taken into account (change uniqueness of variables accordingly)
                        | Interaction_limit(n) -> {rein_ast with settings = {rein_ast.settings with interaction_limit = n}} //this option is currently not taken into account (optimization function?)
                        | Regulation(r) -> {rein_ast with settings = {rein_ast.settings with regulation = r}}
                        | Traj_length(K) -> {rein_ast with settings = {rein_ast.settings with traj_length = K}} //this option is currently not taken into account (paths are unrolled "on-demand")
                        | Updates(u) ->  {rein_ast with settings = {rein_ast.settings with updates = u}}                               
                        | PreventRepeats -> {rein_ast with settings = {rein_ast.settings with preventRepeatedInteractions = true}}    

                    | Motif (name, interactions) ->                                                 
                        let species = 
                            interactions 
                            |> Seq.map(fun (source, target,_,_) -> [source; target]) 
                            |> Seq.concat
                            |> Seq.distinct


                        let M = 
                            { components   = species
                            ; interactions = interactions |> Seq.map REIN.Interaction.Create
                            ; name         = name
                            }

                        {rein_ast with motifs = rein_ast.motifs.Add(name,M)}                        

                ) Problem.empty
            //|> Problem.GenMotifConstraints
            //|> Problem.InitMotifPredicates


    static member ParseAST str = 
            let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString(str)   in                                                     
            try
                Parser.start Lexer.tokenize lexbuf 
                |> Problem.AstToStruct

            with e ->                                
                let pos = lexbuf.EndPos in
                let line = pos.Line in
                let column = pos.Column in
                //et linestr = str.Split('\n').[line] in
                //let len = 10 in
                //let from = if column>len then column-len else 0 in
                //let until = if column<(linestr.Length-len) then column+len else linestr.Length-1 in           
                raise (new System.Exception(System.String.Format("ReinMoCo({0},{1}):error : parsing failed at line {0}, column {1}: {2}",line,column,e.Message)));    
    

    static member ToReinWithoutConstraints (p:Problem) =                 
        { REIN.Problem.interactions = p.interactions
        ; REIN.Problem.species      = p.species
        ; REIN.Problem.predicates   = p.predicates
        ; REIN.Problem.settings     = p.settings
        ; REIN.Problem.constraints  = p.constraints
        ; REIN.Problem.solution     = None
        }            

    static member ToReinWithConstraints (p:Problem) = 
        let problem = 
            p
            |> Problem.GenMotifConstraints
            |> Problem.InitMotifPredicates

        { REIN.Problem.interactions = problem.interactions
        ; REIN.Problem.species      = problem.species
        ; REIN.Problem.predicates   = problem.predicates
        ; REIN.Problem.settings     = problem.settings
        ; REIN.Problem.constraints  = problem.constraints
        ; REIN.Problem.solution     = None
        }            
                    
    //static member ToReinWithoutConstraints (p:Problem) =             
    //    { REIN.Problem.interactions = p.interactions
    //    ; REIN.Problem.species      = p.species
    //    ; REIN.Problem.predicates   = p.motifs |> Map.toSeq |> Seq.fold (fun acc (n,_) -> acc.Add(n, BExpr.BTerm (BTerm.BConst true))) p.predicates //NOTE: fakes motif predicates
    //    ; REIN.Problem.settings     = p.settings
    //    ; REIN.Problem.constraints  = p.constraints
    //    }
        
    static member FromREIN (p:REIN.Problem) =     
        { Problem.interactions = p.interactions
        ; Problem.species      = p.species
        ; Problem.predicates   = p.predicates
        ; Problem.settings     = p.settings
        ; Problem.constraints  = p.constraints
        ; Problem.motifs       = Map.empty
        }                    