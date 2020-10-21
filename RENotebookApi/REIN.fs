namespace Microsoft.Research.RENotebook


type ReinVisualization = 
    static member ModelToGraph (model:Microsoft.Research.REIN.REIN.Problem) = 
        let graph = new Microsoft.Msagl.Drawing.Graph("graph"); 

        model.species
        |> Seq.iter(fun n ->             
            let node = graph.AddNode(n.name)
            node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse

            if n.FE && n.KO then
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Yellow                
            else if n.FE then
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Green
            else if n.KO then             
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Red
            )

        model.interactions
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

        // Creating a bespoke model colouring scheme that I want to use to generate images for publication
        // Signals will be black if active (repurposing the KO label for this...)
        // Nodes will be blue if active (repurposing the KO+FE label for this...)
        // Activatory interactions will be black (not green)
    static member ModelToGraphBlueNodesBlackSignals (model:Microsoft.Research.REIN.REIN.Problem) = 

        let graph = new Microsoft.Msagl.Drawing.Graph("graph")

        model.species
        |> Seq.iter(fun n ->             
            let node = graph.AddNode(n.name)
            node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse                

            if n.FE && n.KO then
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Blue 
                node.Label.FontColor <- Microsoft.Msagl.Drawing.Color.White
            else if n.FE then
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Green
            else if n.KO then             
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Black
                node.Label.FontColor <- Microsoft.Msagl.Drawing.Color.White
            )

        model.interactions
        |> Seq.iter(fun i -> 
            let edge = graph.AddEdge(i.source,i.target)
            
            if i.positive then
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Black //Green
            else
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red            
            
            if not i.definite then
                edge.Attr.AddStyle(Microsoft.Msagl.Drawing.Style.Dashed)
            )

        //Figure out the source nodes so they can go first
        let sourceNodes =
            graph.Nodes
            |> Seq.filter (fun node -> not (Seq.exists (fun (edge:Microsoft.Msagl.Drawing.Edge) -> edge.SourceNode <> node) node.InEdges) )

        let notSourceNodes = System.Linq.Enumerable.Except(graph.Nodes,sourceNodes)

        sourceNodes
        |> Seq.iter (fun sourceNode ->
            notSourceNodes
            |> Seq.iter (fun notSourceNode ->
                graph.LayerConstraints.AddUpDownConstraint(sourceNode,notSourceNode)
                )
            )
//
//        //Rotate to horizontal orientation
//        let settings = Microsoft.Msagl.Layout.Layered.SugiyamaLayoutSettings()
//        settings.Transformation <- Microsoft.Msagl.Core.Geometry.Curves.PlaneTransformation.Rotation(System.Math.PI / 2.0)
//        graph.LayoutAlgorithmSettings <- settings
//
        graph


    // As above but horizontal    
    static member ModelToGraphBlueNodesBlackSignalsHorizontal (model:Microsoft.Research.REIN.REIN.Problem) = 
                
        let graph = new Microsoft.Msagl.Drawing.Graph("graph"); 

        model.species
        |> Seq.iter(fun n ->             
            let node = graph.AddNode(n.name)
            node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse                

            if n.FE && n.KO then
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Blue 
                node.Label.FontColor <- Microsoft.Msagl.Drawing.Color.White
            else if n.FE then
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Green
            else if n.KO then             
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Black
                node.Label.FontColor <- Microsoft.Msagl.Drawing.Color.White
            )

        model.interactions
        |> Seq.iter(fun i -> 
            let edge = graph.AddEdge(i.source,i.target)
            
            if i.positive then
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Black //Green
            else
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red            
            
            if not i.definite then
                edge.Attr.AddStyle(Microsoft.Msagl.Drawing.Style.Dashed)
            )

        //Figure out the source nodes so they can go first
        let sourceNodes =
            graph.Nodes
            |> Seq.filter (fun node -> not (Seq.exists (fun (edge:Microsoft.Msagl.Drawing.Edge) -> edge.SourceNode <> node) node.InEdges) )

        let notSourceNodes = System.Linq.Enumerable.Except(graph.Nodes,sourceNodes)

        sourceNodes
        |> Seq.iter (fun sourceNode ->
            notSourceNodes
            |> Seq.iter (fun notSourceNode ->
                graph.LayerConstraints.AddUpDownConstraint(sourceNode,notSourceNode)
                )
            )

        //Rotate to horizontal orientation
        let settings = Microsoft.Msagl.Layout.Layered.SugiyamaLayoutSettings()
        settings.Transformation <- Microsoft.Msagl.Core.Geometry.Curves.PlaneTransformation.Rotation(System.Math.PI / 2.0)
        graph.LayoutAlgorithmSettings <- settings

        graph

    static member SolutionToGraph (solution:Microsoft.Research.ReasoningEngine.Solution.Solution) = 
        //Note: this function assumes that the solution represents a RE:IN model                
        let interactions = Microsoft.Research.REIN.Export.SolutionToInteractions solution
        //let graph = new Microsoft.Msagl.GraphViewerGdi.GViewer(); 
        let graph = new Microsoft.Msagl.Drawing.Graph("graph");         

        interactions
        |> Seq.iter(fun i -> 
            let edge = graph.AddEdge(i.source,i.target)
            
            if i.positive then
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Green
            else
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red                                
            )
           
        //set node as ovals
        graph.Nodes |> Seq.iter(fun node -> node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse)
        
        graph


type REIN =        
    //IO methods
    static member Load problemString = 
        problemString |> Microsoft.Research.REIN.REIN.ParseAST

    static member LoadFile (problemFilePath:string) = 
        let problemString = System.IO.File.ReadAllText problemFilePath
        Microsoft.Research.REIN.REIN.ParseAST(problemString)
   
    static member ToReil problem = 
        Microsoft.Research.REIN.Translation.Translate(problem)
    
    //Solver methods
    static member Check problem = 
        problem
        |> REIN.ToReil
        |> REIL.Check
        |> Option.map (fun sol -> Microsoft.Research.REIN.Export.Decode problem sol)

    static member CheckAndPrint model = 
        let sol = model |> REIN.Check
        if sol.IsSome then
            "Solution(s) found"
        else
            "No solution(s) found"

    static member Enumerate n problem  =         
        problem 
        |> REIN.ToReil        
        |> REIL.Enumerate n
        |> Array.map (fun sol -> Microsoft.Research.REIN.Export.Decode problem sol)

    static member IdentifyInteractions problem = 
        problem 
        |> Microsoft.Research.REIN.InteractionAnalysis.FindRequiredInteractions
    
    static member FindMinimalModels problem = 
        problem 
        |> Microsoft.Research.REIN.OptimalModels.FindMinimalModels
        |> Seq.map (fun sol -> Microsoft.Research.REIN.Export.Decode problem sol)
    
    static member FindMinimumModels problem = 
        problem 
        |> Microsoft.Research.REIN.OptimalModels.FindMinimumModels
        |> Seq.map (fun sol -> Microsoft.Research.REIN.Export.Decode problem sol)

    //Model manipulation methods
    static member Merge (model:Microsoft.Research.REIN.REIN.Problem) (model':Microsoft.Research.REIN.REIN.Problem) = 
        model.Merge model'

    static member RemoveInteractions (source,target) (model:Microsoft.Research.REIN.REIN.Problem)  = 
        model.RemoveInteractions(source,target)

    static member MkPossible (source,target) (model:Microsoft.Research.REIN.REIN.Problem)  = 
        model.MkPossible (source,target)

    static member MkAllPossible (model:Microsoft.Research.REIN.REIN.Problem) = 
        model.MkAllInteractionsPossible()

    static member MkAllDefinite (model:Microsoft.Research.REIN.REIN.Problem) = 
        model.MkAllInteractionsDefinite()
        
    static member MkDefinite (source,target) (model:Microsoft.Research.REIN.REIN.Problem)  = 
        model.MkDefinite (source,target)

    static member AddAllInteractions (model:Microsoft.Research.REIN.REIN.Problem)  = 
        model.AddAllInteractions()

    static member RemoveInteractionsFrom source (model:Microsoft.Research.REIN.REIN.Problem)  = 
        model.RemoveInteractionsFrom source

    static member RemoveInteractionsTo target (model:Microsoft.Research.REIN.REIN.Problem)  = 
        model.RemoveInteractionsTo target

    static member  AddComponent (name:string) (model:Microsoft.Research.REIN.REIN.Problem) = 
        Microsoft.Research.REIN.REIN.Species.Create(name, None)
        |> model.AddSpecies
    
    static member  AddInteraction source target positive optional (model:Microsoft.Research.REIN.REIN.Problem) = 
        Microsoft.Research.REIN.REIN.Interaction.Create(source,target,positive,optional)
        |> model.AddInteraction
    
    static member  AddDefiniteInteraction (s:string) (s':string) positive (model:Microsoft.Research.REIN.REIN.Problem) = 
        REIN.AddInteraction s s' positive true model

    static member  AddOptionalInteraction (s:string) (s':string) positive (model:Microsoft.Research.REIN.REIN.Problem) = 
        REIN.AddInteraction s s' positive false model
    

    static member  SetInitial experiment name value (model:Microsoft.Research.REIN.REIN.Problem) =   
        let var = Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.BVar(Microsoft.Research.ReasoningEngine.Var.StateVar(experiment,0,name)))
        let sval = Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.BConst(value))
        model.AddConstraint(Microsoft.Research.REIN.REIN.Fact.Create(Microsoft.Research.ReasoningEngine.Constraint.Beq(var,sval),None))
    
    static member  AddConstraint (experiment:string) (step:int) (species:string) value (m:Microsoft.Research.REIN.REIN.Problem) = 
        let v = Microsoft.Research.ReasoningEngine.Var.StateVar(experiment, step, species)
        let cst = Microsoft.Research.ReasoningEngine.Constraint.Beq(Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.BVar v), Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.BConst value))
        let fact = Microsoft.Research.REIN.REIN.Fact.Create(cst,None)
        {m with constraints = Seq.append m.constraints (fact |> Seq.singleton)}
    
    static member  AddFixpoint (experiment:string) (step:int) (m:Microsoft.Research.REIN.REIN.Problem) =     
        let cst = Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.Fixpoint(experiment,step))
        let fact = Microsoft.Research.REIN.REIN.Fact.Create(cst,None)
        {m with constraints = Seq.append m.constraints (fact |> Seq.singleton)}
    
    static member  AddSameConstraint (experiment,step,species) (experiment',step',species') (m:Microsoft.Research.REIN.REIN.Problem) = 
        let v = Microsoft.Research.ReasoningEngine.Var.StateVar(experiment, step, species)
        let v' = Microsoft.Research.ReasoningEngine.Var.StateVar(experiment', step', species')
        let cst = Microsoft.Research.ReasoningEngine.Constraint.Beq(Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.BVar v), Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.BVar v'))
        let fact = Microsoft.Research.REIN.REIN.Fact.Create(cst,None)
        {m with constraints = Seq.append m.constraints (fact |> Seq.singleton)}
    
    static member  AddAllInteractionsWithoutLoops  (m:Microsoft.Research.REIN.REIN.Problem) =
        let interactions = 
            m.interactions
            |> Seq.map(fun i -> i.source, i.target, i.positive)
            |> Set.ofSeq        
        let new_interactions = 
            seq {
                for s in m.species do
                    for s' in m.species do
                        if s.name<>s'.name then
                            if not (interactions.Contains(s.name, s'.name, true)) then 
                                yield Microsoft.Research.REIN.REIN.Interaction.Create(s.name,s'.name,true,false)
                            if not (interactions.Contains(s.name, s'.name, false)) then 
                                yield Microsoft.Research.REIN.REIN.Interaction.Create(s.name,s'.name,false,false)
                }               
        {m with interactions = Seq.append m.interactions new_interactions}

    static member  OneTypeOfInteraction (m:Microsoft.Research.REIN.REIN.Problem) =     
        let cst = 
            m.interactions
            |> Seq.filter (fun i -> not i.definite) 
            |> Seq.map(fun i -> (i.source, i.target), i) 
            |> Seq.groupBy fst
            |> Seq.choose(fun (_,I) ->             
                let Ia = I |> Seq.toArray |> Array.map snd
                if Ia.Length = 2 then            
                    let v = Microsoft.Research.ReasoningEngine.Var.SysVar Ia.[0].var
                    let v' = Microsoft.Research.ReasoningEngine.Var.SysVar Ia.[1].var                
                    Some (Microsoft.Research.ReasoningEngine.Constraint.Not(Microsoft.Research.ReasoningEngine.Constraint.And(Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.BVar v), Microsoft.Research.ReasoningEngine.Constraint.BTerm(Microsoft.Research.ReasoningEngine.Constraint.BVar v'))))
                elif Ia.Length > 2 then            
                    failwith "Too many interactions"
                else
                    None            
                )
            |> Microsoft.Research.ReasoningEngine.Constraint.LAnd
        let fact = Microsoft.Research.REIN.REIN.Fact.Create(cst,None)
        {m with constraints = Seq.append m.constraints (fact |> Seq.singleton)}



    //Visualization methods
    static member DrawSummary (problems:seq<Microsoft.Research.REIN.REIN.Problem>) =     
        let solutions = problems |> Seq.choose (fun p -> p.solution) |> Array.ofSeq
        let n = Seq.length solutions
        let summary = Microsoft.Research.REIN.Export.Summarize solutions
    
        let interactions = summary.Keys
        let row_headers = interactions |> Seq.map(fun i -> i.ToString())
        let col_headers = [0..n-1] |> Seq.map(fun s -> s.ToString())
        let data = 
            interactions
            |> Seq.map(fun i -> 
                let str = i.ToString()
                let v = if i.positive then 1 else -1
                
                let row = 
                    summary.[i]
                    |> Seq.map(fun s -> (s.ToString(),v))
                    |> Map.ofSeq
                
                (str,row)
                )
            |> Map.ofSeq
        let colMap =  [(0,""); (1,"#00FF00");(-1,"#FF0000") ] |> Map.ofSeq            
                
        let content = 
            if n = 0 then "No solutions found" 
            else
                Lib.HTML.ColorTable row_headers col_headers colMap data
        
        Lib.HtmlOutput content
 

    (*static member DrawNetworkWithSize width model = 
        model
        |> Lib.REIN.ModelToGraph
        |> Lib.Jupyter.ShowGraphImg width*)

    static member DrawNetworkWithSizeSVG width model =                                                
        model
        |> ReinVisualization.ModelToGraph
        |> Lib.MSAGL.GraphToSvg width                   
        |> Lib.HtmlOutput

        // Using this to draw networks with custom colours etc
        // Note that the method below is used when you want to orient the graph horizontally, this defaults to vertically
    static member DrawBespokeNetworkWithSizeSVG width model =                                                
        model
        |> ReinVisualization.ModelToGraphBlueNodesBlackSignals
        |> Lib.MSAGL.GraphToSvg width                   
        |> Lib.HtmlOutput

        // As DrawBespokeNetworkWithSizeSVG, except this places input nodes on the left, and orients the graph horizontally
    static member DrawHorizontalNetworkWithSizeSVG width model =                                                
        model
        |> ReinVisualization.ModelToGraphBlueNodesBlackSignalsHorizontal
        |> Lib.MSAGL.GraphToSvg width                   
        |> Lib.HtmlOutput

    static member ObservationsToHtml model =
        model
        |> REIN.ToReil
        //|> Microsoft.Research.ReasoningEngine.Tactics.InlineFixpoint
        |> Microsoft.Research.ReasoningEngine.Tactics.InlinePredicates
        |> REIL.ObservationsToHtml

    static member DrawObservations model =
        model
        |> REIN.ObservationsToHtml              


    static member DrawModelWithSize width model = 
        let network = model |> ReinVisualization.ModelToGraph |> Lib.MSAGL.GraphToSvg width
        let (Lib.HtmlOutput observations) = model |> REIN.ObservationsToHtml

        sprintf "<table><tr><td>%s</td><td>%s</td></tr></table>" network observations
        |> Lib.HtmlOutput

    static member DrawModel model = 
        model 
        |> REIN.DrawModelWithSize 640.0

    static member EnumerateAndDraw n model = 
        model
        |> REIN.Enumerate n
        |> REIN.DrawSummary

    static member DrawRegulationConditions use_optionals (rcDef: Microsoft.Research.REIN.Settings.RegulationConditions)= 
        let rcTable = 
            if use_optionals then 
                Microsoft.Research.REIN.RegulationTables.RegulationConditionTableUsingOptionals rcDef
            else
                Microsoft.Research.REIN.RegulationTables.RegulationConditionTable rcDef

        let m = rcTable |> Seq.length
        let n = rcTable |> Seq.head |> Seq.length        
        let row_headers = [0..m-1] |> Seq.map(fun r -> r.ToString())                
        let col_headers = [0..n-1] |> Seq.map(fun r -> sprintf "<img src=\".\RegulationConditionFigures\%i.png\">" r) |> Array.ofSeq
            
        let data = 
            rcTable
            |> Seq.mapi(fun i row ->
                let rv = 
                    row
                    |> Seq.mapi(fun j v -> 
                        let nv = if v="true" then 1 else 0
                        (col_headers.[j],nv)
                        )
                    |> Map.ofSeq                                                                
                (i.ToString(),rv)
                )    
            |> Map.ofSeq         
        let colMap =  [(0,""); (1,"#FF0000")] |> Map.ofSeq     

        Lib.HTML.ColorTable row_headers col_headers colMap data
        |> Lib.HtmlOutput

    static member DrawInteractions required disallowed = 
        let rStr = required |> Seq.map (fun i -> i.ToString()) |> String.concat "<br>"
        let dStr = disallowed |> Seq.map (fun i -> i.ToString()) |> String.concat "<br>"
        sprintf "<table><tr><th>Required Interactions</th><th>Disallowed Interactions</th></tr><tr><td>%s</td><td>%s</td>" rStr dStr
        |> Lib.HtmlOutput

    //Experimental methods
    static member Infer problem = 
        problem
        |> REIN.ToReil
        |> REIL.Infer
        

    static member ProblemToHtml (rein:Microsoft.Research.REIN.REIN.Problem) = 
        let (Lib.HtmlOutput network) = REIN.DrawBespokeNetworkWithSizeSVG 400.0 rein //REIN.DrawHorizontalNetworkWithSizeSVG 800.0 rein
        let trajectories =             
            match rein.solution with 
            | Some sol -> 
                sol
                |> Microsoft.Research.RENotebook.Lib.TrajectoryVisualization.PlotTrajectories 
                |> Seq.map (fun graph -> graph.GetInlineHtml())
                |> String.concat "<br>"
            | None -> ""
                
        network //+ "<br>" + trajectories
        |> Lib.HtmlOutput

    // Allows you to test a set of models+specifications that are contained within a given directory. It reports back whether solutions exist in each case, or not.
    static member CheckMultipleFiles directory = 
        let dir = new System.IO.DirectoryInfo(directory)
        
        dir.GetFiles("*", System.IO.SearchOption.AllDirectories)
        |> Seq.map(fun file -> 
            let hasSol = (file.FullName |> REIN.LoadFile |> REIN.Check).IsSome
            sprintf "<tr><td>%s</td><td>%A</td></tr>" file.Name hasSol
            )
        |> String.concat ""
        |> sprintf "<table>%s</table>"
        |> Lib.HtmlOutput
            
    static member MakeSynchronous (model:Microsoft.Research.REIN.REIN.Problem) = 
        {model with settings = {model.settings with updates = Microsoft.Research.REIN.Settings.Updates.Synchronous}}