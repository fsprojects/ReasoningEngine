open System

open Microsoft.Research.ReasoningEngine
open Microsoft.Research.RENotebook

type ReilAPI = Microsoft.Research.RENotebook.REIL

let test_inputs = [
    "state int x;";

    """
    state int x;
    update p[k].x := p[k-1].x + 1;
    #test[0].x = 0;
    #test[10].x > 0;
    """;

    """
    state int x;
    update p[k].x := p[k-1].x + 1;
    #test[10].x > 20;
    """;

    """
    state int x;
    update p[k].x := p[k-1].x + 1;
    #test1[10].x > 20;
    #test2[10].x < 5;
    """;

    """
    unique state int temperature;
    unique state bool heatIsOn;
    update
        p[k].temperature := if (p[k-1].heatIsOn) then (p[k-1].temperature + 1) else (p[k-1].temperature - 1),
        p[k].heatIsOn := p[k-1].temperature < 18;

    #test[0].temperature = 20;
    #test[10].temperature < 18;
    """
]

[<EntryPoint>]
let main argv =
    for test_input in test_inputs do
        let model = ReilAPI.Load test_input in
        let solution = ReilAPI.Check model in
        match (solution) with
        | Some(x) -> printfn "Solution (%d paths): %s" x.paths.Count (x.ToString())
        | None -> printfn "No solution."
    0
