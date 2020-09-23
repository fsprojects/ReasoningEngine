module Microsoft.Research.REIN.REINTests
open System
open Microsoft.Research.ReasoningEngine
open Solution


let report str =
    //failwith str //hard errors
    printfn str |> ignore //soft errors






//define a simple model;
//the target gene T has 2 activators and 2 inhibitors
//the regulation condition for T is determined by a parameter
//the regulation conditions for all regulators are not important               
let GenerateDefaultRegCondModel (reg_cond:int) (initial: int array) reg_conds=                         
        
    let activations = 
        "A T positive; \n" + 
        "B T positive; \n"
    in
        
    let repressions = 
        "C T negative; \n" + 
        "D T negative; \n"
    in

    let model_str regulations = 
        //print regulation conditions        
        String.Format("A(0); B(0); C(0); D(0); T({0});\n\n",reg_cond) +        
        regulations +                 
        "\nlet $Initial := {\n" + 
        String.Format("  A={0} and\n",initial.[0]) + 
        String.Format("  B={0} and\n",initial.[1]) + 
        String.Format("  C={0} and\n",initial.[2]) + 
        String.Format("  D={0} and\n",initial.[3]) + 
        String.Format("  T=0\n",initial.[3]) + 
        "};\n" + 
        String.Format("under #traj at 0 $Initial;\n") + 
        String.Format("under #traj at 1 (T=0 or T=1);")
        //String.Format("under #traj at 1 (T=1);")
    in
    let modelA = model_str activations in
    let modelR = model_str repressions in
    let modelT = model_str (activations + repressions) in

    (modelA,modelR,modelT)
in



let GenerateCustomRegCondModel (reg_cond:int) (initial: int array) reg_conds=                         
        
    let activations = 
        "A T positive; \n" + 
        "B T positive; \n"
    in
        
    let repressions = 
        "C T negative; \n" + 
        "D T negative; \n"
    in
      
    let (reg0,_) = Seq.head reg_conds in    
        
    let model_str regulations = 
        //print regulation conditions                
        (reg_conds |> Seq.fold(fun acc (n,e) -> acc + "let " + n + " := {" + e + "};\n") "") + "\n\n" + 
        String.Format("A({0}); B({0}); C({0}); D({0}); T({1});\n",reg0, (Seq.nth reg_cond reg_conds |> fst)) +   
        regulations +                 
        "\nlet $Initial := {\n" + 
        String.Format("  A={0} and\n",initial.[0]) + 
        String.Format("  B={0} and\n",initial.[1]) + 
        String.Format("  C={0} and\n",initial.[2]) + 
        String.Format("  D={0} and\n",initial.[3]) + 
        String.Format("  T=0\n",initial.[3]) + 
        "};\n" + 
        String.Format("under #traj at 0 $Initial;\n") + 
        String.Format("under #traj at 1 (T=0 or T=1);")
        //String.Format("under #traj at 1 (T=1);")
    in
    let modelA = model_str activations in
    let modelR = model_str repressions in
    let modelT = model_str (activations + repressions) in

    (modelA,modelR,modelT)
in






let TestReinRegCond (reg_conds:(string*string) seq) modelGenerator =             
    let mx = (Seq.length reg_conds)-1 in
    let testRegCond id ic = 
        //printfn "%i,%A" id ic
        let (reinA,reinR,rein) = modelGenerator id ic reg_conds in        
          
        let extract_result (results: Solution option)= 
            match results with 
            |Some(sol) -> sol.paths.["#traj"].states.[1].["T"]="true"                        
            |None -> failwith "UNSAT at regulation condition %i at %A" id ic
        in
     
        let resA = Solver.BMC (Translation.Parse(reinA)) |> extract_result in
        let resR = Solver.BMC (Translation.Parse(reinR)) |> extract_result in
        let res = Solver.BMC (Translation.Parse(rein))   |> extract_result in

        (resA,resR,res)        
    in
    
    let testRegCondAll id = 
        let res = Array.create 16 ((0,0,0,0),(false,false,false))
        //printfn "-----------------------\nRegulation condition %i:\n-----------------------" id;
        for a in [0..1] do
            for b in [0..1] do
                for c in [0..1] do
                    for d in [0..1] do
                        let cid = 8*d + 4*c + 2*b + 1*a in
                        res.[cid] <- (a,b,c,d),(testRegCond id [|a;b;c;d|])

        res
    in


    //generate testing data:
    //data has format logic(int) * initial conditions (bool*bool*bool*bool) * result (bool*bool*bool), 
    //  where result captures a network with activators only * repressors only * both
    let data = Seq.fold(fun acc i -> Seq.append(acc) (Seq.map(fun (r1,r2) -> i,r1,r2) (testRegCondAll i))) Seq.empty [0..mx] in    
    //Seq.iter(fun (l, (a,b,c,d), (rA,rR,r)) -> printfn "%i,\t%i,%i,%i,%i,\t%b,%b,%b" l a b c d rA rR r) data

    //analyze data
    //1) make sure that it doesn't matter which activator/repressor is enabled    
    for rc in [0..mx] do
        let fd = Seq.filter(fun (l,_,_) -> l=rc) data  |> List.ofSeq in
        let m = fd.Length in
        for i in [0..m-2] do 
            for j in [i+1..m-1] do
                let _,(ai,bi,ci,di),(r1i,r2i,r3i) = fd.[i] in
                let _,(aj,bj,cj,dj),(r1j,r2j,r3j) = fd.[j] in
               
                let Ai,Aj,Ri,Rj = (ai+bi), (aj+bj), (ci+di), (cj+dj) in                                
                if (Ai=Aj) && not(r1i=r1j) then report "Different results with equal number of activators!"  //same number of activators --> check the results for no repressors
                if (Ri=Rj) && not(r2i=r2j) then report "Different results with equal number of repressors"  //same number of repressors --> check the results for no activators
                if (Ai=Aj) && (Ri=Rj) && not(r3i=r3j) then report "Different results with equal number of activators and repressors"  //same number of both activators and repressors --> check the results for both                    

    //if the check above passed, filter data to keep only the sum of activators/repressors  
    let data = Seq.map(fun (l,(a,b,c,d),r) -> l,(a+b,c+d),r) data |> Seq.distinct in            


    let b2s b = if b then "1" else "0" in
    //2) arrange the results         
    Seq.iteri(fun rc (n,e) ->        
        let fd = Seq.filter(fun (l,_,_) -> l=rc) data  |> Seq.map(fun (l, n,(r1,r2,r3)) -> n,(b2s r1, b2s r2, b2s r3))  in//select the data for the given logic

        let fd1 = Seq.fold(fun (a0,a1,a2) ((nA,_),(rA,_,_))-> 
                                match nA with
                                |0 -> if a0 = "" then (rA,a1,a2) else if rA = a0 then (a0,a1,a2) else failwith "Values disagree"
                                |1 -> if a1 = "" then (a0,rA,a2) else if rA = a1 then (a0,a1,a2) else failwith "Values disagree"
                                |2 -> if a2 = "" then (a0,a1,rA) else if rA = a2 then (a0,a1,a2) else failwith "Values disagree"
                            ) ("","","") fd in
        
        let fd2 = Seq.fold(fun (a0,a1,a2) ((_,nR),(_,rR,_))-> 
                                match nR with
                                |0 -> if a0 = "" then (rR,a1,a2) else if rR = a0 then (a0,a1,a2) else failwith "Values disagree"
                                |1 -> if a1 = "" then (a0,rR,a2) else if rR = a1 then (a0,a1,a2) else failwith "Values disagree"
                                |2 -> if a2 = "" then (a0,a1,rR) else if rR = a2 then (a0,a1,a2) else failwith "Values disagree"
                            ) ("","","") fd in       
        

        //let red s = Seq.reduce(fun a b -> a + ", " + b) s in
        let (a1, a2, a3) = fd1 in
        let (r1,r2,r3) = fd2 in


        let res = rc.ToString() + ", " +  n + "," + e + ", " + 
                  a1 + ", " + a2 + ", " + a3 + ", " +
                  r1 + ", " + r2 + ", " + r3 + ", " + 
                  (Seq.map(fun (n,(r1,r2,r3)) -> r3) fd |> Seq.reduce(fun a b -> a + ", " + b))
        in

        printfn "%s" res
        ) reg_conds








let GenerateCompareRegCondModel (reg_cond:int) reg_conds=                                 
    let activations = 
        "A1 T1 positive; \n" + 
        "B1 T1 positive; \n" + 
        "C1 T1 positive; \n" + 

        "A2 T2 positive; \n" + 
        "B2 T2 positive; \n" + 
        "C2 T2 positive; \n"
    in
        
    let repressions = 
        "D1 T1 negative; \n" + 
        "E1 T1 negative; \n" + 
        "F1 T1 negative; \n" + 

        "D2 T2 negative; \n" + 
        "E2 T2 negative; \n" + 
        "F2 T2 negative; \n"
    in
      
    let (reg0,_) = Seq.head reg_conds in    
        
    let model_str regulations = 
        //print regulation conditions                
        (reg_conds |> Seq.fold(fun acc (n,e) -> acc + "let " + n + " := {" + e + "};\n") "") + "\n\n" + 
        String.Format("A1({0}); B1({0}); C1({0}); D1({0}); E1({0}); F1({0}); T1({1});\n A2({2}); B2({2}); C2({2}); D2({2}); E2({2}); F2({2}); T2({3});\n",0,reg_cond,reg0, (Seq.nth reg_cond reg_conds |> fst)) +   
        regulations +                 
        "\nlet $Initial := {\n" + 
        "  A1=A2 and\n" + 
        "  B1=B2 and\n" + 
        "  C1=C2 and\n" + 
        "  D1=D2 and\n" + 
        "  E1=E2 and\n" + 
        "  F1=F2 and\n" + 
        "  T1=T2\n" + 
        "};\n" + 
        String.Format("under #traj at 0 $Initial;\n") + 
        String.Format("under #traj at 1 not (A1=A2 and B1=B2 and C1=C2 and D1=D2 and E1=E2 and F1=F2 and T1=T2);")        
    in
    let modelA = model_str activations in
    let modelR = model_str repressions in
    let modelT = model_str (activations + repressions) in

    (modelA,modelR,modelT)
in





let reg_conds = 
        ["@default0",  "available activators > 0 and available activators = total activators and total repressors = 0";  //And(NotRepressible s, AllActivatorsPresent s);          
         "@default1",  "available activators > 0 and total repressors = 0"; //And(NotRepressible s, Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s));
         "@default2",  "available activators > 0 and (total repressors = 0 and available activators = total activators) or (total repressors > 0 and available repressors = 0)"; //Or(And(NoRepressorsPresent s, Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s)), And(NotRepressible s, AllActivatorsPresent s));         
         "@default3",  "available activators > 0 and available repressors = 0"; //And(Or(NoRepressorsPresent s, NotRepressible s), Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s));
         "@default4",  "available activators > 0 and available activators = total activators and (available repressors = 0 or available repressors < total repressors)"; //And(AllActivatorsPresent s, Not(AllRepressorsPresent s));         
         "@default5",  "available activators > 0 and ((total repressors = 0 and available activators <= total activators) or (total repressors > 0 and available repressors < total repressors and available activators = total activators))"; // Or(And(NotRepressible s, SomeButNotAllActivatorsPresent s), And(AllActivatorsPresent s, Not(AllRepressorsPresent s)));  //logic 5
         "@default6",  "available activators > 0 and ((total repressors = 0 and available activators = total activators) or available repressors < total repressors)"; //Or(And(SomeButNotAllActivatorsPresent s, Or(NoRepressorsPresent s, SomeButNotAllRepressorsPresent s)), And(AllActivatorsPresent s, Not(AllRepressorsPresent s)));
         "@default7",  "available activators > 0 and (total repressors = 0 or available repressors < total repressors)"; // And(Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s), Not(AllRepressorsPresent s));
         "@default8",  "available activators > 0 and available activators = total activators"; // AllActivatorsPresent s; 
         "@default9",  "available activators > 0 and (available activators = total activators or (total repressors = 0 and available activators < total activators))"; // Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, NotRepressible s));
         "@default10", "available activators > 0 and (available activators = total activators or (total repressors > 0 and available repressors = 0))"; // Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, NoRepressorsPresent s)); //logic 10
         "@default11", "available activators > 0 and (available repressors = 0 or available activators = total activators)";// Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Or(NotRepressible s, NoRepressorsPresent s)));
         "@default12", "available activators > 0 and (available activators = total activators or available repressors < total repressors)"; // Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Or(NoRepressorsPresent s, SomeButNotAllRepressorsPresent s)));  //logic 12
         "@default13", "available activators > 0 and (available activators = total activators or total repressors = 0 or available repressors < total repressors)";// Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Not(AllRepressorsPresent s)));
         "@default14", "available activators > 0 and (available activators = total activators or total repressors > 0)";// Or(AllActivatorsPresent s, And(SomeButNotAllActivatorsPresent s, Not(NotRepressible s)));  //logic 14
         "@default15", "available activators > 0"; // Or(SomeButNotAllActivatorsPresent s, AllActivatorsPresent s);  //logic 15
         "@default16", "total activators = 0 and available repressors < total repressors";// And(SomeButNotAllRepressorsPresent s, NotInducible s); //logic 16 (for TCF3 and MEKERK - part of signaling and activated by default)
         "@default17", "total activators = 0 and available repressors = 0"; // And(NoRepressorsPresent s, NotInducible s); //logic 17 (for TCF3 and MEKERK - part of signaling and activated by default)
         ]



let TestCompareRegCond()  =    
    let extract_result rc results = 
        match results with 
        |Some(q) -> let vars = (Map.toSeq q.varmap) |> Seq.map(fun (n,k) -> n.ToString(),k) |> Map.ofSeq in
                    
                    let get k n i = vars.[String.Format("#traj[{0}].{1}{2}",k,n,i)]
                    let get_state k i = 
                        String.Format("A{0}: {1}; B{0}:{2}; C{0}:{3}; D{0}:{4}; E{0}:{5}; F{0}:{6}; T{0}:{7}",i, get k  "A" i, get k "B" i,get k "C" i,get k "D" i,get k "E" i, get k "F" i,get k "T" i)                        
                    in                                                            
                    printfn "=> Discrepency in regulation condition %s" rc 
                    //printfn " --> initial conditions (%s) and (%s)" (get_state 0 1) (get_state 0 2)
                    //printfn " --> reached (%s) and (%s)" (get_state 1 1) (get_state 1 2)                    
        |None -> printfn "Regulation condition %s:\tOK" rc //no solutions found -> the logics agree
    in           
    Seq.iteri(fun i (n,rc) -> 
        let reinA,reinR, rein= GenerateCompareRegCondModel i reg_conds in
        let resA = Solver.BMC (Translation.Parse(reinA)) |> extract_result (n + "(A)") in
        let resR = Solver.BMC (Translation.Parse(reinR)) |> extract_result (n + "(R)") in
        let res = Solver.BMC (Translation.Parse(rein))   |> extract_result n in


        //output benchmarks to files
        System.IO.File.WriteAllText(n + "_activators.in",reinA);
        System.IO.File.WriteAllText(n + "_repressors.in",reinR);
        System.IO.File.WriteAllText(n + ".in",rein);

        ()               
        ) reg_conds
    


let TestReinCustomRegCond() =         
    TestReinRegCond reg_conds GenerateCustomRegCondModel

let TestReinDefaultRegCond() = 
    let reg_conds = Seq.map(fun i -> let name = "default" + i.ToString() in (name,name)) [0..17] in    
    TestReinRegCond reg_conds GenerateDefaultRegCondModel
  


