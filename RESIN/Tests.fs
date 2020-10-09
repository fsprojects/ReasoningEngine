module Microsoft.Research.RESIN.RESINTests

open System
open Microsoft.Research.ReasoningEngine



let TestRESINRegulationConditions =     
    //define a simple model;
    //the target gene T has 2 activators and 2 inhibitors
    //the regulation condition for T is determined by a parameter
    //the regulation conditions for all regulators are not important               
    let model (reg_cond:int) (initial: int array)=                         
        
        //print regulation conditions        
        String.Format("A(0); B(0); C(0); D(0); T({0});\n",reg_cond) +
        "define $cell := {\n"+
        "  A T positive, \n" + 
        "  B T positive, \n" + 
        "  C T negative, \n" + 
        "  D T negative \n" + 
        "};\n" + 
        "let $Initial := {\n" + 
        String.Format("  A={0} and\n",initial.[0]) + 
        String.Format("  B={0} and\n",initial.[1]) + 
        String.Format("  C={0} and\n",initial.[2]) + 
        String.Format("  D={0} and\n",initial.[3]) + 
        String.Format("  T=0\n",initial.[3]) + 
        "};\n" + 
        String.Format("under #traj at 0 $Initial;\n") + 
        String.Format("under #traj at 1 (T=0 or T=1);")
    in
    


    let testRegCond id ic = 
        let resin = model id ic in
        //printfn "%s" resin

        let reil = RESIN.Parse(resin) in  
        //printfn "%A" (reil)
    
        let results = Solver.BMC reil in

        match results with 
        |Some(q) ->
            let vars = (Map.toSeq q.varmap) |> Seq.map(fun (n,k) -> n.ToString(),k) |> Map.ofSeq in
            let res = vars.["#traj[1].T"] in                        
            let ic_str = Seq.map(fun i -> i.ToString()) ic |> Seq.reduce(fun a b -> a + ", " + b) in
            printfn "%s : %s" ic_str res;
        |None -> printfn "UNSAT at regulation condition %i with %A" id ic
    in

    let testRegCondAll id = 
        printfn "-----------------------\nRegulation condition %i:\n-----------------------" id;
        for a in [0..1] do
            for b in [0..1] do
                for c in [0..1] do
                    for d in [0..1] do
                        testRegCond id [|a;b;c;d|]

    in

    Seq.iter testRegCondAll [0..17]    
    //testRegCondAll 18