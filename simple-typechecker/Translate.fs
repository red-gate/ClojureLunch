module Translate

open Expressions
open Microsoft.FSharp.Quotations.Patterns

let rec translate expr = 
  match expr with
  | Application (exp1, exp2) -> App(translate exp1, translate exp2)
  | Let (var, exp1, exp2) -> Let(var.Name, translate exp1, translate exp2)
  | LetRecursive ([var, exp1], exp2) -> Let(var.Name, translate exp1, translate exp2)
  | Lambda (x, exp1) -> Abs(x.Name, translate exp1)
  | Var var -> Exp.Var(var.Name)
  | _ -> failwithf "Can't handle %A " expr

translate <@@ let rec bottom = (fun x -> bottom x) : int -> int in bottom @@>

translate <@@ let id = fun x -> x : int -> int in id @@>
