module Microsoft.Research.REIN.REIN


open Microsoft.Research.ReasoningEngine.Constraint
open Parser


type Interaction = {
    source: string;
    target: string;
    positive: bool;
    definite: bool;
    var: string;
    } with
    static member Create(source,target,positive,definite) =         
        { source=source
        ; target=target
        ; positive=positive
        ; definite=definite
        ; var = if definite then "" else ((if positive then "Pos_" else "Neg_") + source + "_" + target)} //note: change var to be an option?
    member this.MkDefinite() =
        { source = this.source
        ; target = this.target
        ; positive = this.positive
        ; definite = true
        ; var = this.var
        }
    member this.MkPossible() =
        { source = this.source
        ; target = this.target
        ; positive = this.positive
        ; definite = false
        ; var = (if this.positive then "Pos_" else "Neg_") + this.source + "_" + this.target
        }
    override this.ToString() = 
        let arrow = if this.positive then ">" else "|"
        let symbol = (if this.definite then "--" else "-?-") + arrow
        this.source + symbol + this.target    
    member this.ToModelString() =         
        this.source + "\t" + this.target + "\t" + 
        (if this.positive then "positive" else "negative") + "\t" +
        (if this.definite then "\t" else "optional")            


type Species = 
    { name: string
    ; reg_conds: int list option
    ; KO: bool
    ; FE: bool
    ; NeedsActivator: bool //if set to true, an additional constraint requiring activators for this node will be included
    ; lVar: string   //regulation logic variable name
    ; KoVar: string  //KO variable name
    ; FeVar: string  //FE variable name
    } with
    static member Create(name,reg_conds,hasKO,hasFE,needsAct) =    
        { name=name
        ; reg_conds = reg_conds
        ; KO=hasKO
        ; FE=hasFE
        ; NeedsActivator=needsAct
        ; lVar =  "L_" + name
        ; KoVar = "KO_"+ name
        ; FeVar = "FE_"+ name        
        }
    static member Create(name,reg_conds,hasKO,hasFE) = Species.Create(name,reg_conds,hasKO,hasFE, false)
    static member Create(name,reg_conds) = Species.Create(name,reg_conds, false, false)    
    override this.ToString() = 
        let perturbations = 
            if this.KO || this.FE || this.NeedsActivator then 
                "[" + 
                (if this.KO then "-" else "") + 
                (if this.FE then "+" else "") + 
                (if this.NeedsActivator then "!" else "") + 
                "]"
            else ""
        let reg_cond = 
            match this.reg_conds with 
            | None -> ""
            | Some rcs -> "(" + (rcs |> Seq.map(fun i -> i.ToString()) |> Seq.reduce(fun a b -> a + ", "+ b)) + ")"                           
        this.name + perturbations + reg_cond

type Fact = 
    { expr: BExpr
    ; info: string option
    } with
    static member Create(e,i) = {expr=e; info = i}
    override this.ToString() = 
        this.expr.ToString() + 
        match this.info with
        |None -> ""
        |Some d -> sprintf " \"%s\"" (d.Replace("\"",""))
        
type Problem = {
    species: seq<Species>;
    interactions: seq<Interaction>    
    predicates: Map<string,BExpr>;    
    constraints: seq<Fact>;    
    settings: Settings.Settings;
    solution: Microsoft.Research.ReasoningEngine.Solution.Solution option
    } with
    static member empty = {species = Seq.empty; interactions = Seq.empty; predicates = Map.empty; constraints = Seq.empty; settings = Settings.Settings.Default; solution = None}
    member this.AddInteraction(i) = {this with interactions = Seq.append(this.interactions) [i]} 
    member this.AddSpecies(s) = {this with species = Seq.append(this.species) [s]} 
    member this.AddConstraint(f) = {this with constraints = Seq.append(this.constraints) [f]} 
    member this.AddPredicate(n,e) = {this with predicates = this.predicates.Add(n,e)} 
    member this.InlinePredicates() = {this with constraints = Seq.map(fun f -> {f with expr = f.expr.Inline this.predicates}) this.constraints}
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
                            yield Interaction.Create(s.name,s'.name,true,false)
                        if not (interactions.Contains(s.name, s'.name, false)) then 
                            yield Interaction.Create(s.name,s'.name,false,false)
                }               
        {this with interactions = Seq.append this.interactions new_interactions}



    (* TODO: This function requires more error checking. 
    For example, it is possible to merge two models where the same species (by name) appears with different allowed regulation conditions
    Also, a definite interaction can be included on top of an optional interactions (which is actually possible in the language as well)

    Note that the settings of the first model are currently preserved!
    *)
    member this.Merge (model:Problem) =        
        { species = Seq.append this.species model.species |> Seq.distinct
        ; interactions =  Seq.append this.interactions model.interactions |> Seq.distinct
        ; predicates = this.predicates |> Map.fold(fun map n e -> map.Add(n,e)) model.predicates
        ; constraints = Seq.append this.constraints model.constraints |> Seq.distinct
        ; settings = this.settings
        ; solution = None
        }





    override this.ToString() = 
        let settings = @"//Settings" + "\r\n" + this.settings.ToString() + "\r\n"                     

        let species = 
            if not (Seq.isEmpty this.species) then
                @"//Species" + "\r\n" + 
                Seq.fold(fun acc p -> acc + p.ToString() + ";\r\n") "" this.species + "\r\n\r\n" 
            else ""
        let interactions = 
            if not (Seq.isEmpty this.interactions) then
                @"//Interactions" + "\r\n" + 
                Seq.fold(fun acc (p:Interaction) -> acc + p.ToModelString() + ";\r\n") ""  this.interactions + "\r\n\r\n" 
            else ""
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
        settings + species + interactions + predicates + constraints


let AstToStruct spec = 
        spec
        |> Seq.fold(fun rein_ast x -> 
            match x with          
              | Interaction(a,b,c,d) -> {rein_ast with interactions = Seq.append(rein_ast.interactions) [Interaction.Create(a,b,c,d)]}
              | Assert(e,description) ->           
                    let fact = {expr = e; info = description} in
                    {rein_ast with constraints = Seq.append(rein_ast.constraints) [fact]}
              | Assign(n,e) -> {rein_ast with predicates = rein_ast.predicates.Add(n,e)}          
              | Species(a,b,c,d,e) -> {rein_ast with species = Seq.append(rein_ast.species) [Species.Create(a,b,c,d,e)]}               
              | Directive(d) -> 
                    match d with
                    | Uniqueness(u) ->  {rein_ast with settings = {rein_ast.settings with uniqueness = u}} //this option is currently not taken into account (change uniqueness of variables accordingly)
                    | Interaction_limit(n) -> {rein_ast with settings = {rein_ast.settings with interaction_limit = n}} //this option is currently not taken into account (optimization function?)
                    | Regulation(r) -> {rein_ast with settings = {rein_ast.settings with regulation = r}}
                    | Traj_length(K) -> {rein_ast with settings = {rein_ast.settings with traj_length = K}} //this option is currently not taken into account (paths are unrolled "on-demand")
                    | Updates(u) ->  {rein_ast with settings = {rein_ast.settings with updates = u}}       
                    | PreventRepeats -> {rein_ast with settings = {rein_ast.settings with preventRepeatedInteractions = true}}       
            ) Problem.empty

let ParseAST str = 
        let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString(str)   in                                                     
        try
            Parser.start Lexer.tokenize lexbuf 
            |> AstToStruct
        with e ->                                
            let pos = lexbuf.EndPos in
            let line = pos.Line in
            let column = pos.Column in
            let linestr = str.Split('\n').[line] in
            let len = 10 in
            let from = if column>len then column-len else 0 in
            let until = if column<(linestr.Length-len) then column+len else linestr.Length-1 in           
            raise (new System.Exception(System.String.Format("RE:IN({0},{1}):error : parsing failed at line {0}, column {1}: {2}",line+1,column,e.Message)));    
