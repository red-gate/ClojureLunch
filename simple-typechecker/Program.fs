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

// Define the type representing types

// And a means to get new type variables

// We'll also need type environments and type schemas

// The basic technique requires unification and substitution so let's write those on some simple examples

// instantiate to get fresh variables when we look something up
// We can now put the identity in the type environment and look it up

// generalise to make a schema when we let bind something
// We can now generalise the identity function

// Now we just put it all together

// Let's try

let a = fun x -> x // Abstract

let a : int -> int = (fun x -> x) (fun x -> x) //Apply


