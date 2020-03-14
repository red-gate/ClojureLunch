module EvaluatorExamples

open Evaluator
open Examples
open Runtime

let envPlus =
    Map.ofList
        [ ("+", Plus)
          ("if", IF0) ]

let r1 = eval ex1 envPlus
let r2 = eval ex2 envPlus
let r3 = eval ex3 envPlus
let r4 = eval ex4 envPlus
let r5 = eval ex5 envPlus
let r6 = eval ex6 envPlus
let r7 = eval ex7 envPlus
let r8 = eval ex8 envPlus
let r9 = eval ex9 envPlus
let r10 = eval ex10 envPlus
