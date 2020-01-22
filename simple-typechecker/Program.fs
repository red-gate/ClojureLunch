// Abstract syntax for pure functional language
// We'll need abstraction, application, anonymous functions, let bindings

type Exp = 
  | Plus
  | Plus1 of int
  | Int of int
  | Var of string
  | Abs of string * Exp  // fun x -> x* 2
  | App of Exp * Exp  // f 20
  | Let of string * Exp * Exp // let x = 20 in x * 30

type Env = Map<string, Exp>

// 20
let ex1 = Int(20)

// 10 + 20
let ex2 = App(App(Var("+"),Int(10)), Int(20))


// fun x -> x
let ex3 = Abs("x",Var("x"))

// let id = fun x -> x in id
let ex4 = Let("id",Abs("x", Var("x")), Var("id"))

let rec eval x env = 
  match x with
  | Int n -> x
  | Var x -> match Map.tryFind x env with
      | Some x -> x
      | None -> failwith "Name not found"
  | App(f,v) -> 
     let l = eval f env
     let r = eval v env
     match l,r with
     | Plus, Int x -> Plus1 x
     | Plus1 x, Int y -> Int(x+y)
     | Abs (n, ex), v -> eval v (Map.add n ex env)
  | Abs _ -> x
  | Let (variable, def, exp) -> eval exp (Map.add variable def env)    

let envPlus = Map.ofList [("+", Plus)]

eval ex3 envPlus

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

let a = fun x -> x // Abstract

//let a : int -> int = (fun x -> x) (fun x -> x) //Apply


