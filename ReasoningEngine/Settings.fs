module Microsoft.Research.ReasoningEngine.Settings

type Directive = 
    |Enumerate of int//EnumerationType
    |Abstraction of AbstractionType

and Z3Encodings = 
    |BitVector of uint32
    |Integer

and EnumerationType = 
    | First
    | All
    | Limit of int

and AbstractionType = 
    |Default   //T(x,x') iff there is a reaction at state x that produces x' 
    |Stutter   //T(x,x') iff there is a reaction that can execute multiple times to produce x'
    |Connected //T(x,x') for all x,x'

and Options = {
    encoding          : Z3Encodings;
    abstraction       : AbstractionType;
    enumerate         : int;//EnumerationType;
    addNullReaction   : bool;
    min_traj_length   : int;    
    }

let defaultSettings = {
    encoding          = Integer;
    abstraction       = Default;
    enumerate         = 100;
    addNullReaction   = true;
    min_traj_length   = 0;    
    }

let ParseSettings (directives:seq<Directive>) (currentSettings:Options) =                 
        let opt = ref currentSettings in        
        Seq.iter(fun directive -> match directive with
                                    |Enumerate(e) -> opt:={!opt with enumerate=e;}
                                    |Abstraction(a) -> opt:={!opt with abstraction=a;}) directives;
        !opt      