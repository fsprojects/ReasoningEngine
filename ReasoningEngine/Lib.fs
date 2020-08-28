module Microsoft.Research.ReasoningEngine.Lib


let seq2str s = Seq.map(fun e -> e.ToString()) s |> Seq.reduce(fun a b -> a + ", " + b)



let benchmark fname f p = 
    printf "Function %s..." fname
    let stamp = System.DateTime.Now in
    let res = f p in
    printfn "%f min" (System.DateTime.Now-stamp).TotalMinutes;
    res
