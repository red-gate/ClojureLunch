// Abstract syntax for pure functional language
// We'll need abstraction, application, annymous functions, let bindings

// We'll target 

let a = 
  let id x = x
  (id 2, id true)

// but let's start with 

let a = 
  let id x = x
  id 2

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

let a : int -> int = (fun x -> x) (fun x -> x) //Apply


