module Microsoft.Research.ReasoningEngine.Formula

type Formula = 
    |Temporal
    |Logical
    |Atom of string //does this make sense?
and Logical = 
    |Neg of Formula
    |And of Formula * Formula
    |Or of Formula * Formula
    |Imp of Formula * Formula
    |Eq of Formula * Formula
    |Const of bool    
and Temporal =
    |AX of Formula    
    |EX of Formula        
    |AF of Formula
    |EF of Formula
    |AG of Formula
    |EG of Formula
    |AU of Formula * Formula
    |EU of Formula * Formula