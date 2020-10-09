namespace Microsoft.Research.RENotebook


open Microsoft.Research.ReasoningEngine.Constraint
open Microsoft.Research.REINMoCo
open Lib

type Fact = Microsoft.Research.REIN.REIN.Fact

type ReinMoCo =         
    

    static member DrawMotif context width (motif:Motif) = 
        let node_filter  = 
            if context then fun (c:string) -> true
            else fun (c:string) -> c<>"Context"

        let interaction_filter = 
            if context then fun (c:Microsoft.Research.REIN.REIN.Interaction) -> true
            else fun (c:Microsoft.Research.REIN.REIN.Interaction) -> c.source<>"Context" && c.target<>"Context"

        let graph = new Microsoft.Msagl.Drawing.Graph("graph"); 
        motif.components
        |> Seq.filter node_filter
        |> Seq.iter(fun n ->             
            let node = graph.AddNode(n)
            node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse
            )

        motif.interactions
        |> Seq.filter interaction_filter
        |> Seq.iter(fun i -> 
            let edge = graph.AddEdge(i.source,i.target)
            
            if i.positive then
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Green
            else
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red            
            
            if not i.definite then
                edge.Attr.AddStyle(Microsoft.Msagl.Drawing.Style.Dashed)
            )

        graph
        |> Lib.MSAGL.GraphToSvg width

    //NOTE: context is never shown for this function
    static member DrawAllMotifs width (motifs:seq<Motif>) = 
        let graph = new Microsoft.Msagl.Drawing.Graph("graph"); 

        motifs
        |> Seq.iteri(fun i motif ->                         
            motif.components 
            |> Seq.filter(fun c -> c<>"Context") 
            |> Seq.map(fun c -> sprintf "%s_%i" c i) //unique names
            |> Seq.iter(fun n ->             
                let node = graph.AddNode(n)
                node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Circle
                node.LabelText <- ""               
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Black
                )

            motif.interactions 
            |> Seq.filter(fun c -> c.source<>"Context" && c.target <> "Context") 
            |> Seq.map(fun c -> {c with source = sprintf "%s_%i" c.source i; target = sprintf "%s_%i" c.target i})
            |> Seq.iter(fun i -> 
                let edge = graph.AddEdge(i.source,i.target)            
                edge.Attr.LineWidth <- 3.5                
                if i.positive then
                    edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Green                    
                else
                    edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red            
            
                if not i.definite then
                    edge.Attr.AddStyle(Microsoft.Msagl.Drawing.Style.Dashed)
                )
            )
        
        let graphSVG = graph |> Lib.MSAGL.GraphToSvg width
 
        MSAGL.EnsureGraphGeometry graph

        let nodesMap = graph.Nodes |> Seq.map(fun n -> n.Id,n.GeometryNode.Center) |> Map.ofSeq

        //TODO: align labels with motifs if we use this approach?
        let motifLabels = 
            motifs
            |> Seq.mapi(fun i motif ->                         
                let X, Y = 
                    motif.components 
                    |> Seq.filter(fun c -> c<>"Context") 
                    |> Seq.map(fun c -> 
                        let p = nodesMap.[sprintf "%s_%i" c i]
                        p.X, p.Y
                        )
                    |> Seq.toList
                    |> List.unzip

                //motif.name, X |> List.average, Y |> List.average
                sprintf "<text x=\"%f\" y=\"%f\" class=\"small\">%s</text>" (X |> List.min) 165.0 motif.name.[1..]
                )
            |> String.concat "\n"
        
        graphSVG.Replace("</svg></svg>",motifLabels+"</svg>")

    static member DrawMotifs context width (p:Problem) =                         
        p.motifs 
        |> Map.toSeq
        |> Seq.map (fun (name,m) -> m |> ReinMoCo.DrawMotif context width|> sprintf "<div>%s<br>%s<div>" name)        
        |> String.concat ""
        |> Lib.HtmlOutput

    //IO methods
    static member Load problemString = 
        problemString |> Problem.ParseAST

    static member LoadFile (problemFile:string) = 
        let problemStream = (new System.IO.StreamReader(problemFile))        
        problemStream.ReadToEnd() 
        |> Problem.ParseAST        
   
    static member ToReil problem = 
        problem
        |> Problem.ToReinWithConstraints
        |> Microsoft.Research.REIN.Translation.Translate

    static member ToReinWithConstraints (p:Problem) =  p |> Problem.ToReinWithConstraints
    
    static member ToReinWithoutConstraints (p:Problem) =  p |> Problem.ToReinWithoutConstraints
    

    static member AddMotif name (motif:Motif) (m:Problem) = 
        let m' = {m with motifs = m.motifs.Add(name, motif)}        
        let m'' = {m' with predicates = m'.predicates.Add(name, Problem.MotifToConstraints m' name)}
        Problem.InitMotifPredicates m''
    
    //Convert a REIN model to a ReinMoCo model and add the given motif definitions
    static member AddMotifs (motifs:seq<Motif>) (p:Microsoft.Research.REIN.REIN.Problem) = 
        motifs
        |> Seq.fold(fun m motif -> ReinMoCo.AddMotif motif.name motif m)  (p  |> Problem.FromREIN)        

    
    static member CheckEachMotif (p:Microsoft.Research.REIN.REIN.Problem) (motifs:seq<Microsoft.Research.REINMoCo.Motif>) (outputFile:string option) =                   
        let writeToLog (s:string) = 
            System.Console.Error.WriteLine(s)
            match outputFile with
            | Some f -> System.IO.File.AppendAllText(f, s + "\r\n")
            | None -> ()
        
        motifs
        |> Seq.map (fun motif ->             
            let result = Microsoft.Research.REINMoCo.Analysis.CheckMotif p motif
            Microsoft.Research.REINMoCo.Analysis.ReinMoCoResult.ToTsvRow result |> writeToLog
            result
            )
            
            
    static member CheckEachMotifAndShowResults (p:Microsoft.Research.REIN.REIN.Problem) (motifs:seq<Microsoft.Research.REINMoCo.Motif>) (outputFile:string option) =   
        ReinMoCo.CheckEachMotif p motifs outputFile
        |> Microsoft.Research.REINMoCo.Analysis.ReinMoCoResult.ToHtmlTable
        |> Lib.HtmlOutput