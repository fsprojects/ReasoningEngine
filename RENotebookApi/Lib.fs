﻿module Microsoft.Research.RENotebook.Lib

open Microsoft.Msagl.Miscellaneous

type HtmlOutput = HtmlOutput of string


type MSAGL = 

    ///A side-effect of running this mutates the graph such that it contains the required geometry
    static member internal EnsureGraphGeometry (graph:Microsoft.Msagl.Drawing.Graph) =

        //let renderer = new Microsoft.Msagl.GraphViewerGdi.GraphRenderer(graph);
        //renderer.CalculateLayout();

        graph.CreateGeometryGraph()

        let geometryGraph = graph.GeometryGraph

        let font_family = ref null
        if not (SixLabors.Fonts.SystemFonts.TryFind("Arial", font_family)) then
            if not (SixLabors.Fonts.SystemFonts.TryFind("Liberation Sans", font_family)) then begin
                let ffit = SixLabors.Fonts.SystemFonts.Families.GetEnumerator()
                if (ffit.MoveNext()) then
                    font_family := ffit.Current
            end
        let font = new SixLabors.Fonts.Font(!font_family, 16.f)
        let options = new SixLabors.Fonts.RendererOptions(font)

        //use bitmap = new System.Drawing.Bitmap(1000, 1000)
        //let graphics = System.Drawing.Graphics.FromImage bitmap

        //use sf = System.Drawing.StringFormat.GenericTypographic

        //let stringFont = new System.Drawing.Font("Arial", 16.f)

        //Are nodes definitely ordered?
        (graph.Nodes, graph.GeometryGraph.Nodes)
        ||> Seq.iter2 (fun graphNode geometryNode ->


            let labelText = graphNode.LabelText

            //let width = 5.0 + (float) labelText.Length * 12.0 //80 as a default?

            //let sizeF = graphics.MeasureString(labelText, stringFont, 10000, sf)

            //let doubleWidth = graphics.MeasureString(labelText+labelText, stringFont, 100000, sf).Width;
            //let singleWidth = graphics.MeasureString(labelText, stringFont, 100000, sf).Width;
            //let width = (double)doubleWidth - (double)singleWidth;

            let sizeF_six = SixLabors.Fonts.TextMeasurer.Measure(labelText, options);
            let width = (double)sizeF_six.Width

            geometryNode.BoundaryCurve <- Microsoft.Msagl.Drawing.NodeBoundaryCurves.GetNodeBoundaryCurve(graphNode, width, 25.0)

            graphNode.Label.Height <- 20.0
            graphNode.Label.Width <- width
            )
        LayoutHelpers.CalculateLayout(geometryGraph, graph.LayoutAlgorithmSettings, null)
    

    static member GraphToSvg width (graph:Microsoft.Msagl.Drawing.Graph) =        
        MSAGL.EnsureGraphGeometry graph

        let stream = new System.IO.MemoryStream()
        let svgWriter = new Microsoft.Msagl.Drawing.SvgGraphWriter(stream,graph)
                
        svgWriter.Write()               
        
        stream.Flush()
        stream.Close()

        let height = width*(graph.Height/graph.Width)

        let asSvg =
            stream.ToArray()             
            |> System.Text.Encoding.ASCII.GetString

        //let patched = asSvg.Replace("font-size=\"16\"", "font-size=\"20\"")

        sprintf "<svg width=\"%f\" height=\"%f\" viewBox=\"0 0 %f %f\">%s</svg>" width height graph.Width graph.Height asSvg

type Jupyter =     
    static member ShowGraphSvg width graph =                         
        graph
        |> MSAGL.GraphToSvg width
        |> HtmlOutput
                


type HTML = 

    //the dimensions of all parameters should be consistent
    //data is integer valued map of maps
    //empty values are treated as 0
    static member ColorTable (row_headers:string seq) (col_headers:string seq) (colMap:Map<int,string>) (data:Map<string,Map<string,int>>) = 
        
        let headers = sprintf "<tr><th style=\"border: 1px solid black\"></th>%s</tr>" (col_headers |> Seq.map(fun i -> sprintf "<th style=\"border: 1px solid black\">%s</th>" i) |> Seq.reduce(fun a b -> a + "\n" + b))        
        row_headers
        |> Seq.map(fun r -> 
            col_headers
            |> Seq.map(fun c -> 
                let color = 
                    if (not (data.ContainsKey r)) || (not (data.[r].ContainsKey c)) then 
                        colMap.[0]
                    else
                        colMap.[data.[r].[c]]
                if color = "" then 
                    sprintf "<td style=\"border: 1px solid black\"></td>"
                else
                    sprintf "<td bgcolor=\"%s\" style=\"border: 1px solid black\"></td>" color
                )
            |> Seq.reduce(fun a b -> a + "\n" + b)
            |> sprintf "<tr><td style=\"border: 1px solid black\">%s</td>%s</tr>" r
            )
        |> Seq.reduce(fun a b -> a + "\n" + b)
        |> sprintf "<table style=\"border: 3px solid black\">\n%s\n%s\n</table>" headers

type REIN  =
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

type RESIN = 
    static member SwitchesToGraph (model:Microsoft.Research.RESIN.RESIN.Problem) = 
        let graph = new Microsoft.Msagl.Drawing.Graph("graph"); 

        model.cells
        |> Seq.iter(fun cell ->             
            let node = graph.AddNode(cell.cname)
            node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse            
            )

        model.switches        
        |> Seq.iter(fun s -> graph.AddEdge(s.cell1, s.cell2) |> ignore)

        graph

type TrajPlotSettings = 
    { offset    : float
    ; spread    : float
    ; plotLine  : XPlot.Plotly.Graph.Line
    ; plotMode  : string
    ; opacity   : float  //plot opacity    
    }

type TrajectoryVisualization = 
    static member DefaultTrajPlotSettings = 
        let line = XPlot.Plotly.Graph.Line(shape = "hvh") //hv or vh?
        line.width <- 3.0

        { offset   = 5.0
        ; spread   = 2.0
        ; plotLine = line
        ; plotMode = "lines" //lines+markers or just lines?
        ; opacity  = 1.0
        }
    
    static member GetPathPlot (settings:TrajPlotSettings) name (path:Microsoft.Research.ReasoningEngine.Solution.Path) = 
         let pathArray = path.states |> Map.toArray
         let cells, keys = 
             pathArray.[0] |> snd |> Map.toArray |> Array.map fst |> Array.partition (fun k -> k.[0] = '&')
     
         let max_t = pathArray |> Array.map fst |> Array.max
              
         let layout = XPlot.Plotly.Layout.Layout()          
         layout.shapes <- []          
     
         let mutable ylabels = []
         
         let AddLine y = 
             let s = XPlot.Plotly.Graph.Shape(x0=0.0,x1=max_t,y0=y,y1=y)
             let l = XPlot.Plotly.Graph.Line()
             l.color <- "rgba(68, 68, 68, 0.10)"
             s.line <- l          
             layout.shapes <- Seq.append (Seq.singleton s) layout.shapes            
     
         //add lines for cells
         let cells_plot = 
             if not (Array.isEmpty cells)  then
                 for i in [0..cells.Length-1] do 
                     let y =  (-settings.offset - settings.spread * (float i))
                     ylabels <- (y,cells.[i])::ylabels         
                     //y |> AddLine


                //add cell plots
                 let cells_X, cells_Y = 
                     pathArray 
                     |> Array.map(fun (t, state) ->              
                         let cell = 
                             cells 
                             |> Array.map(fun c -> c, state.[c]) 
                             |> Array.filter (fun (_,v) -> v = "true")
                             |> fun x -> x.[0]
                             |> fst 

                         let vv = Array.findIndex (fun x -> x = cell) cells |> float                          
                         float t, -settings.offset - settings.spread*vv                          
                         )
                     |> Array.unzip
                     //|> MakeDigital

                 let plot = XPlot.Plotly.Graph.Scatter(x=cells_X, y=cells_Y,name="cell type",line=settings.plotLine,mode=settings.plotMode)
                 plot.opacity <- settings.opacity
                 [|plot|]
             else
                 Array.empty
     
     
         //add lines for signals
         for i in [0..keys.Length-1] do                
             let y = settings.offset * (float i)
             ylabels <- (y,keys.[i])::ylabels         
             //AddLine y
             AddLine (1.0 + y)
         
     
         //Add cell yaxis labels
         let yaxis = XPlot.Plotly.Graph.Yaxis()
         let yvals, ytxt = ylabels |> List.unzip
         yaxis.ticktext <- ytxt
         yaxis.tickvals <- yvals
         yaxis.ticks <- ""
         yaxis.showticklabels <- true     
         layout.yaxis <- yaxis  
     
   
         //add signal plots
         let final_plots = 
             keys
             |> Array.mapi(fun i key -> 
                 let X,Y = 
                     pathArray 
                     |> Array.map(fun (t, state) -> 
                         let v = if state.[key]="true" then 1.0 else 0.0
                         let v' = settings.offset*(float i) + v
                         float t,v')
                     |> Array.unzip
                     //|> MakeDigital
             
                 let plot = XPlot.Plotly.Graph.Scatter(x=X, y=Y,name=key,line=settings.plotLine,mode=settings.plotMode)         
                 plot.opacity <- settings.opacity
                 plot
                 )                 
             |> Array.append cells_plot
         final_plots, layout


    static member PlotPath (settings:TrajPlotSettings) name (path:Microsoft.Research.ReasoningEngine.Solution.Path) = 
         let plots, layout =  TrajectoryVisualization.GetPathPlot settings name path

         plots
         |> XPlot.Plotly.Chart.Plot     
         |> XPlot.Plotly.Chart.WithLayout layout
         |> XPlot.Plotly.Chart.WithTitle name
         |> XPlot.Plotly.Chart.WithLegend false
    
    static member PlotTrajectories (sol:Microsoft.Research.ReasoningEngine.Solution.Solution) =           
     sol.paths
     |> Map.toArray
     |> Array.map(fun (name,path) ->  TrajectoryVisualization.PlotPath TrajectoryVisualization.DefaultTrajPlotSettings name path)                  
     
 
    static member PlotTrajectories (sols:Microsoft.Research.ReasoningEngine.Solution.Solution[]) =           
     
     let settings = {TrajectoryVisualization.DefaultTrajPlotSettings with opacity = 0.2 }//1.0/(float sols.Length)

     sols
     |> Array.map(fun sol -> 
         sol.paths
         |> Map.toArray
         |> Array.map(fun (name,path) ->              
            name, TrajectoryVisualization.GetPathPlot settings name path)
         )
     |> Array.concat
     |> Seq.groupBy fst
     |> Seq.map(fun (name, P) -> 
        let layout = P |> Seq.head |> snd |> snd
        let plots = P |> Seq.map (snd >> fst) |> Array.ofSeq |> Array.concat
       
        plots
        |> XPlot.Plotly.Chart.Plot     
        |> XPlot.Plotly.Chart.WithLayout layout
        |> XPlot.Plotly.Chart.WithTitle name
        |> XPlot.Plotly.Chart.WithLegend false
        )

    static member SolutionToPlot prefix (sol:Microsoft.Research.ReasoningEngine.Solution.Solution) = 
        let line = XPlot.Plotly.Graph.Line(shape="hvh")
        sol.paths
        |> Map.toArray
        |> Array.collect (fun (path, p) -> 
            p.states
            |> Map.toArray
            |> Array.collect(fun (t,state) -> 
                state
                |> Map.toArray
                |> Array.map (fun (var,v) -> 
                    t, var, v
                    )
                )
            |> Array.groupBy (fun (_,var,_) -> var)
            |> Array.map(fun (var, L) -> 
                let T, X = 
                    L 
                    |> Array.map(fun (t,_,x)-> 
                        let x' = 
                            if x.Trim().ToLower() = "true" then 1
                            elif x.Trim().ToLower() = "false" then 0
                            else int x
                        t, x')
                    |> Array.unzip
    
                XPlot.Plotly.Graph.Scatter(x=T,y=X,name=sprintf "%s%s.%s" prefix path var,line=line)
                )
            )
                
    
    //static member PlotTrajectories (x:Microsoft.Research.ReasoningEngine.Solution.Solution option)= 
    //    match x with 
    //    | Some sol -> 
    //        sol
    //        |> SolToPlot ""
    //        |> XPlot.Plotly.Chart.Plot
    //        |> XPlot.Plotly.Chart.WithLegend true
    //        |> Display
    
    //    | None -> ()
        
    static member PlotSolutionTrajectories (solutions:Microsoft.Research.ReasoningEngine.Solution.Solution[])= 
        if Array.isEmpty solutions then 
            HtmlOutput "No solutions found!"
        else
            solutions
            |> Array.mapi(fun i s ->TrajectoryVisualization.SolutionToPlot (sprintf "Solution %i " i) s)
            |> Array.concat
            |> XPlot.Plotly.Chart.Plot
            |> XPlot.Plotly.Chart.WithLegend true
            |> fun x -> x.GetInlineHtml()
            |> HtmlOutput

    static member PlotSolutionTrajectories (solOpt: Microsoft.Research.ReasoningEngine.Solution.Solution option) = 
        match solOpt with
        | Some sol -> TrajectoryVisualization.PlotSolutionTrajectories [|sol|]
        | None     -> HtmlOutput "No solutions found!"
