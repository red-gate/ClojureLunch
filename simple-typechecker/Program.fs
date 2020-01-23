// Abstract syntax for pure functional language
// We'll need abstraction, application, anonymous functions, let bindings

type Exp = 
  | Int of int
  | Var of string
  | Abs of string * Exp  // fun x -> x* 2
  | App of Exp * Exp  // f 20
  | Let of string * Exp * Exp // let x = 20 in x * 30

and Runtime =
  | Plus
  | Plus1 of int
  | RInt of int
  | RAbs of string * Exp * REnv
  
and REnv = Map<string, Runtime>

// 20
let ex1 = Int(20)

// 10 + 20
let ex2 = App(App(Var("+"),Int(10)), Int(20))

// fun x -> x
let ex3 = Abs("x",Var("x"))

// let id = fun x -> x in id
let ex4 = Let("id",Abs("x", Var("x")), Var("id"))

// let f = 
//   let x = 10
//   fun y -> x
// f  
let ex5 = Let("f", Let("x", Int(10), Abs("y", Var("x"))),
               Var("f"))

// let f = 
//   let x = 10
//   fun y -> x
// let x = 50
// f  

let ex6 = Let("f", Let("x", Int(10), Abs("y", Var("x"))),
               Let("x", Int(50), Var("f")))

// let f = 
//   let x = 10
//   fun y -> x
// let x = 50
// f 6 

let ex7 = Let("f", Let("x", Int(10), Abs("y", Var("x"))),
               Let("x", Int(50), App(Var("f"), Int(6))))

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
  //printf " %A %A \n" l r
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

let envPlus = Map.ofList [("+", Plus)]

let r1 = eval ex1 envPlus
let r2 = eval ex2 envPlus
let r3 = eval ex3 envPlus
let r4 = eval ex4 envPlus
let r5 = eval ex5 envPlus
let r6 = eval ex6 envPlus
let r7 = eval ex7 envPlus

// We'll target 

//let a = 
//  let id x = x
//  (id 2, id true)

// but let's start with 

//let a = 
//  let id x = x
//  id 2

// We'll do this the functinal way (though this makes some things a lot harder)
//  - typically you'd use mutable slots to represent the substituions that we'll end up doing by rewrites

// Define the type representing types

// And a means to get new type variables

// We'll also need type environments and type schemes

// The basic technique requires unification and substitution so let's write those on some simple examples
//  - what's unification (and the most general unifier)
//  - how do we apply a subsitution to a type/type scheme/type environment
//  - and compose substituions

// instantiate to get fresh variables when we look something up
// We can now put the identity in the type environment and look it up

// generalise to make a schema when we let bind something
// We can now generalise the identity function

// Now we just put it all together

// The typecheck worker function will return a type and a substitution

// Let's try

//let a = fun x -> x // Abstract

//let a : int -> int = (fun x -> x) (fun x -> x) //Apply


