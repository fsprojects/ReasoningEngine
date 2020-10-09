module Microsoft.Research.REINMoCo.MotifGenerator

type private Graph = Set<int>[] // adj. list representation (output edges are stored as a set)
    
let private allVisited (graph:Graph)= 
    let visited = Array.init graph.Length (fun _ -> false)
    let rec rdfs i  =
        visited.[i] <- true
        graph.[i] |> Set.filter (fun i -> not visited.[i]) |> Set.iter rdfs
    rdfs 0
    visited |> Array.forall id

let private reverse (graph:Graph)= 
    let graph' = Array.init graph.Length (fun _ -> Set.empty)
        
    graph
    |> Array.iteri(fun i targets -> 
        targets 
        |> Set.iter(fun j -> graph'.[j] <- graph'.[j].Add i))
        
    graph'
   

let private IsGraphConnected (graph:Graph) = (allVisited graph)

let private IsGraphStronglyConnected (graph:Graph) = 
    (allVisited graph) && allVisited (reverse graph)



type private Interactions = int[][]           // NxN matrix  {+1.0 :positive, -1.0:negative, 0.0:no interaction

/// Rotates a list by one place forward.
let private rotate lst =
    List.tail lst @ [List.head lst]

/// Gets all rotations of a list.
let private getRotations lst =
    let rec getAll lst i = if i = 0 then [] else lst :: (getAll (rotate lst) (i - 1))
    getAll lst (List.length lst)

/// Gets all permutations (without repetition) of specified length from a list.
let rec private getPerms n lst = 
    match n, lst with
    | 0, _ -> seq [[]]
    | _, [] -> seq []
    | k, _ -> lst |> getRotations |> Seq.collect (fun r -> Seq.map ((@) [List.head r]) (getPerms (k - 1) (List.tail r)))


let private NetworkToGraph (I:Interactions) :Graph =     
    Array.init I.Length (fun i -> I.[i] |> Array.indexed |> Array.filter (fun (_,v) -> v <> 0) |> Array.map fst |> Set.ofSeq)                        

let private NetworkToGraphUndirected (I:int[][]) :Set<int>[] =     
    let G = I |> NetworkToGraph    
    G
    |> Array.mapi (fun i g -> //add reverse interactions
        let g' = 
            [0..I.Length-1]
            |> Seq.filter(fun j -> G.[j].Contains i)
            |> Set.ofSeq            
        g'+g
        )

let private IsNetworkConnected (I:Interactions) =     
    I
    |> NetworkToGraphUndirected
    |> IsGraphStronglyConnected

let private IsNetworkStronglyConnected (I:Interactions) =     
    I
    |> NetworkToGraph
    |> IsGraphStronglyConnected


let private Isomorphisms (I:Interactions) : Set<Interactions>=         
    getPerms I.Length [0..I.Length-1] //permute interactions
    |> Seq.map(fun order -> 
        let lookup x = order.[x]        
        I |> Array.map (Array.permute lookup) |> Array.permute lookup 
        )    
    |> Set.ofSeq



type Connectivity = 
    | StronglyConnected
    | Connected
    | AnyConnectivity

type SelfLoops = 
    | Removed  //do not include any self loops
    | Optional //include all self loops as optional
    | Definite //permute self loops as part of different motifs


let FilterIsomorphisms (networks: seq<Interactions>) = 
    let mutable to_add = networks |> Set.ofSeq 
    
    //return the unique networks
    [| while not (Set.isEmpty to_add) do                    
        //pick a network
        let I = to_add |> Set.toArray |> Array.head                           
        to_add <- to_add - (Isomorphisms I)
        yield I
    |]        

let private GenerateNetworks (connectivity:Connectivity) (selfloops:SelfLoops) (species:string[]) : Interactions[]=
    let N = species.Length           
    let allNetworks = 
        let rec GenerateInteractionsVector n = 
            if n <= 1 then 
                [[-1]; [0]; [1]]
            else
                GenerateInteractionsVector (n-1)
                |> List.map(fun v -> [-1::v; 0::v; 1::v])
                |> List.concat            
            
        GenerateInteractionsVector (int ((float N)**2.0))
        |> List.map (List.splitInto N >> List.map (List.toArray) >> List.toArray)                   
          
    allNetworks 
    |> List.filter 
        (match selfloops with 
        | Removed | Optional -> fun I -> Array.init I.Length (fun i -> I.[i].[i]=0) |> Array.forall id //keep only networks that have no self loops
        | Definite -> (fun _ -> true)
        )
    |> List.filter  //Filter connected networks
        (match connectivity with 
        | StronglyConnected -> IsNetworkStronglyConnected
        | Connected -> IsNetworkConnected
        | AnyConnectivity -> (fun _ -> true)
        )
    |> FilterIsomorphisms
            

    
      
      
let SelfLoops (components:string[]) =
    components
    |> Array.collect(fun c -> 
        [| Microsoft.Research.REIN.REIN.Interaction.Create(c,c,true,false)
           Microsoft.Research.REIN.REIN.Interaction.Create(c,c,false,false)                   
        |])
let ContextInteractions (components:string[]) =
    components
    |> Array.collect(fun c -> 
        [| Microsoft.Research.REIN.REIN.Interaction.Create(c,"Context",true,false)
           Microsoft.Research.REIN.REIN.Interaction.Create(c,"Context",false,false)
           Microsoft.Research.REIN.REIN.Interaction.Create("Context",c,true,false)
           Microsoft.Research.REIN.REIN.Interaction.Create("Context",c,false,false)
        |])

let NetworkToInteractions  (components:string[])  (I:Interactions)= 
    I 
    |> Array.indexed
    |> Array.collect(fun (i, II) -> 
        II
        |> Array.indexed
        |> Array.collect(fun (j, n) -> 
            if n<0 then //negative interaction
                [|Microsoft.Research.REIN.REIN.Interaction.Create(components.[i],components.[j],false,true)|]
            elif n>0 then //positive interaction
                [|Microsoft.Research.REIN.REIN.Interaction.Create(components.[i],components.[j],true,true)|]
            else //no interaction
                Array.empty
        )
    )    
    
//NOTE: assuming that nodes are not unique (e.g. not matching ones in the ABN) and all isomorphisms can be ignored
let GenerateMotifs anyContext connectivity selfloops (components:string[]) =
    let contextInteractions = if anyContext then ContextInteractions components else Array.empty           
    let selfInteractions = 
        match selfloops with 
        | Optional -> SelfLoops components            
        | _ -> Array.empty    

    components
    |> GenerateNetworks connectivity selfloops
    |> Array.mapi(fun m network -> 
        let interactions = 
            network
            |> NetworkToInteractions components                
            |> Array.append contextInteractions
            |> Array.append selfInteractions

        { Microsoft.Research.REINMoCo.Motif.components   = components
        ; Microsoft.Research.REINMoCo.Motif.interactions = interactions
        ; Microsoft.Research.REINMoCo.Motif.name = sprintf "$Motif_%i_%i" components.Length m
        })

    

    

//NOTE: assuming that nodes are not unique (e.g. not matching ones in the ABN) and all isomorphisms can be ignored   
type MotifType = 
    | FeedForward
    | FeedBack

let GenerateStructuredMotifs (t:MotifType) anyContext selfloops (components:string[]) =
    let contextInteractions = if anyContext then ContextInteractions components else Array.empty           
    let selfInteractions = 
        match selfloops with 
        | Optional -> SelfLoops components            
        | _ -> Array.empty    
    
    let n = components.Length
    let m = int (2.0**(float n)) - 1
    
    let networks = 
        [|0..m|]
        |> Array.map(fun x -> 
            let signs = System.Convert.ToString(x,2).PadLeft(n,'0')  //interaction signs
            let network = Array.init n (fun _ -> Array.init n (fun _ -> 0))
            match t with 
            | FeedForward -> network.[0].[n-1] <- if signs.[n-1]='1' then 1 else -1
            | FeedBack -> network.[n-1].[0] <- if signs.[n-1]='1' then 1 else -1        
            signs |> Seq.take (n-1) |> Seq.iteri(fun i c -> network.[i].[i+1] <- if c = '1' then 1 else -1)
            network
        )
        |> FilterIsomorphisms

    networks
    |> Array.mapi(fun k network -> 
        let interactions = 
            network
            |> NetworkToInteractions components
            |> Seq.append contextInteractions
            |> Seq.append selfInteractions
            |> Array.ofSeq

        let prefix = match t with FeedBack -> "FB" | FeedForward -> "FF"
        { Microsoft.Research.REINMoCo.Motif.components   = components
        ; Microsoft.Research.REINMoCo.Motif.interactions = interactions
        ; Microsoft.Research.REINMoCo.Motif.name = sprintf "$Motif%s_%i_%i" prefix components.Length k
        })                
        