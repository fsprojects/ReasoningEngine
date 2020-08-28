module Microsoft.Research.ReasoningEngine.Tests
open System
open Constraint
open Var

//simplify tests
let SimplifyTest1 = 
    Console.WriteLine("\nSimplify test:\n")
    let exp = And(BTerm(BConst(false)),Or(BTerm(BConst(true)),BTerm(BConst(false))))
    printf "Expr:\t\t%A\nSimplified:\t%A\n\n" exp (BSimplify(exp))    



let SimplifyTest2 = 
    Console.WriteLine("\nSimplify test:\n")
    let exp = And(BTerm(BVar(SysVar("A"))),BTerm(BVar(SysVar("A")))) in
    printf "Expr:\t\t%A\nSimplified:\t%A\n\n" exp (BSimplify(exp))       