module Translate

open Expressions
open Microsoft.FSharp.Quotations.Patterns

let rec translate expr =
    match expr with
    | Application(exp1, exp2) -> App(translate exp1, translate exp2)
    | Let(var, exp1, exp2) -> Let(var.Name, translate exp1, translate exp2)
    | LetRecursive([ var, exp1 ], exp2) -> Let(var.Name, translate exp1, translate exp2)
    | Lambda(x, exp1) -> Abs(x.Name, translate exp1)
    | Var var -> Exp.Var(var.Name)
    | Value(x, _) -> Int(downcast x: int)
    | Call(_, op, args) ->
        match op.Name, args with
        | "op_Addition", [ a; b ] -> App(App(Var "+", translate a), translate b)
    | _ -> failwithf "Can't handle %A " expr
