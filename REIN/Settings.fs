module Microsoft.Research.REIN.Settings

type Uniqueness = 
    | Interactions   //unique solutions based on interactions only
    | Model          //unique solutions based on interactions and regulation conditions
    | Path of string * int // unique solutions based on the first k steps of a specifc path
    | All            //unique solutions based on interactions, regulation conditions and all paths
    
    override this.ToString() = 
        match this with
        | Interactions -> "interactions"
        | Model -> "full"
        | All -> "paths"
        | Path (s,n) -> sprintf "path %s %i" s n

type RegulationConditions = 
    | Legacy //regulation conditions from the Stem Cell (Science) paper
    | Cardinality //legacy conditions redefined using cardinality, with monotonic fix and threshold conditions
    | Default //updated regulation conditions including thresholds
    | NoThresholds //updated regulation conditions without thresholds    
    | Terms //basic regulation condition terms (for debugging)
    override this.ToString() =
        match this with 
        | Legacy  -> "legacy"
        | Cardinality -> "cardinality"
        | Default -> "default"
        | NoThresholds -> "noThresholds"                
        | Terms -> "terms"
    
(*
Since some of the regulation conditions are generated algorithmically, it is hard to find the number of conditions at run-time
Thus, the maximum regulation condition value (e.g. 0..17 for a total of 18 regulation conditions in Default) is hard-coded.
*)
(* 
 SJ: Note that I have just added a new threshold function, which is required for the yeast cell cycle model work. Thus the max reg condition value
 for Cardinality is 19. 
*)
let maxConditions = [(Legacy,17); (Cardinality,19); (Default,19); (NoThresholds,17); (Terms,11)] |> Map.ofSeq

type Updates = 
    | Synchronous
    | Asynchronous
    override this.ToString() = 
        match this with 
        | Synchronous -> "sync"
        | Asynchronous -> "async"

type Settings = {
    uniqueness: Uniqueness;
    interaction_limit: int option;
    regulation: RegulationConditions
    traj_length: int
    updates: Updates
    preventRepeatedInteractions : bool //if true, an additional constraint is generate to prevent the inclusion of multiple interactions between the same components
    } with
    static member Default = 
        { uniqueness= Interactions
          interaction_limit = None
          regulation = Default
          traj_length = 20
          updates = Synchronous
          preventRepeatedInteractions = false
        }

    override this.ToString() = 
        let uniqueness = 
            if this.uniqueness <> Settings.Default.uniqueness then 
                sprintf "directive uniqueness %s;" (this.uniqueness.ToString()) 
            else ""

        let limit = 
            if this.interaction_limit <> Settings.Default.interaction_limit then 
                match this.interaction_limit with
                | Some n -> sprintf "directive limit %i;" n
                | None -> ""
            else ""

        let regulation = 
            if this.regulation <> Settings.Default.regulation then 
                sprintf "directive regulation %s;" (this.regulation.ToString())
            else
                ""

        let length = 
            if this.traj_length <> Settings.Default.traj_length then
                sprintf "directive length %i;" this.traj_length
            else
                ""

        let updates = 
            if this.updates <> Settings.Default.updates then
                sprintf "directive updates %s;" (this.updates.ToString()) 
            else
                ""

        let preventRepeatedInteractions = 
            if this.preventRepeatedInteractions<> Settings.Default.preventRepeatedInteractions then 
                sprintf "directive preventRepeatedInteractions;"
            else
                ""
        [uniqueness; limit; regulation; length; updates; preventRepeatedInteractions] |> String.concat "\r\n"

                