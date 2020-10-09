module Microsoft.Research.RESIN.Export
open Microsoft.Research.RESIN.RESIN
open Microsoft.Research.REIN.REIN


//BY: This function could be generalized to convert REIL solutions to concrete RESIN models
let SolutionToInteractions (sol:Microsoft.Research.ReasoningEngine.Solution.Solution) =     
    sol.vars
    |> Map.toSeq
    |> Seq.filter(fun (name,_) -> name.Contains("Pos_") || name.Contains("Neg_")) //select interaction variables only
    |> Seq.filter(fun (_,value) -> value = "true") //selected interactions only
    |> Seq.map(fun (name,_ ) ->  //keep only the variable name and split into cell * positive? * source * target        
        let i = name.Split('_')
        let positive = (i.[1]= "Pos")
        (i.[0],positive,i.[2],i.[3]))   
    |> Seq.groupBy(fun (c,_,_,_) -> c) //group by cell name
    |> Seq.map(fun (c,is) -> 
        let interactions = 
            is
            |> Seq.map(fun (_,pos,source,target) -> 
                    { source = source
                    ; target = target
                    ; positive = pos
                    ; definite = true //all interactions are marked as definite in a solution
                    ; var = ""} //note that since this is a solution, variable names should not be required                      
                )
        (c, interactions)
        )
    |> Map.ofSeq      


//return a data structure that represents a summary of a number of solutions
let Summarize  (solutions:seq<Microsoft.Research.ReasoningEngine.Solution.Solution>) = 
    let summary = new System.Collections.Generic.Dictionary<Interaction,Set<string*int>>()
    
    //a function that adds a given solution id to the summary for a given interaction
    let addSolution (cell,id) interaction = 
        if not (summary.ContainsKey(interaction)) then
            summary.Add(interaction, Set.empty) //create a slot for the interaction            
        
        //if interaction.definite then //the interaction was included
        let cs = summary.[interaction] //store the current list of solutions containing the interaction
        summary.Remove(interaction)|> ignore    //temporarily remove the interaction from the summmary
        summary.Add(interaction, Set.add (cell,id) cs ) //add a new list where the current solution is appended                            

    //construct the summary
    if Seq.isEmpty solutions then //no solutions -> return an empty summary
        summary
    else               
        //construct a sparse dictionary representation of the summary
        solutions
        |> Seq.iteri(fun id solution -> 
            solution
            |> SolutionToInteractions 
            |> Map.toSeq 
            |> Seq.iter(fun (cell, interactions) ->  //for each cell
                interactions 
                |> Seq.iter(fun i -> addSolution (cell,id) i))) //add each interaction
        summary

    //Note that this function currently does not preserve any specifications
let CellsToRein (model:RESIN.Problem) =            
    model.cells
    |> Seq.map(fun c -> {Problem.empty with species = model.species; interactions = c.interactions}) 