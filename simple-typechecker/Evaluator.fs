module Evaluator

open Expressions
open Runtime


let rec eval x env = 
  match x with
  | Int n -> RInt n
  | Var x -> match Map.tryFind x env with
              | Some x -> x
              | None -> failwith "Name not found"
  | App(f,v) -> 
     let l = eval f env
     apply l v env 
  | Abs (v,body) -> RAbs(v,body, env)
  | Let (variable, def, exp) -> 
       let rdef = eval def env 
       eval exp (Map.add variable rdef env)    

and apply l v env =
  printf " %A %A \n" l v
  match l with
  | Plus ->
     let (RInt y) = eval v env
     Plus1 y
  | Plus1 x -> 
      let (RInt y) = eval v env
      RInt (x+y)
  | RAbs (n, ex, cenv) ->
      let arg = eval v env 
      eval ex (Map.add n arg cenv)
  | IF0 ->
      let (RInt y) = eval v env
      if y = 0 then IFTrue0 else IFFalse0
  | IFTrue0 -> IFTrue1 (eval v env)
  | IFTrue1 x -> x
  | IFFalse0 -> IFFalse1
  | IFFalse1 -> eval v env

