module TypecheckerExamples

open Expressions
open Typechecker

let envWithid = Map.ofList ([ ("id", Scheme([ "a" ], TFun(TVar "a", TVar "a"))) ])

typecheckInEnv (Var "id") envWithid
typecheckInEnv (Abs("x", Var "x")) Map.empty
typecheckInEnv (Abs("x", Int 2)) Map.empty
typecheckInEnv (Let("id", Abs("x", Var "x"), Var "id")) Map.empty
typecheckInEnv (Let("id", Abs("x", Var "x"), App(Var "id", Int 3))) Map.empty
typecheckInEnv (Let("id", Abs("x", Var "x"), (App(App(Abs("p", Abs("q", Var("q"))), App(Var "id", Int 3)), Var "id"))))
    Map.empty

// Type check a recursive function

//let rec f = fun x -> if x = 0 then 0 else 1 + f(x-1)

//let rec bottom = fun x -> bottom x

let envWithIfAndPlus =
    Map.ofList
        ([ ("+", Scheme([], TFun(TInt, TFun(TInt, TInt))))
           ("if", Scheme([ "a" ], TFun(TInt, TFun(TVar "a", TFun(TVar "a", TVar "a"))))) ])

typecheckInEnv (LetRec("x", Abs("x", Var "x"), Var "x")) envWithIfAndPlus
typecheckInEnv (LetRec("bottom", Abs("x", App(Var "bottom", Var "x")), Var "bottom")) envWithIfAndPlus
typecheckInEnv
    (LetRec
        ("f",
         Abs
             ("x",
              App
                  (App(App(Var "if", Var "x"), Int 0),
                   App(App(Var "+", Int 1), App(Var "f", App(App(Var "+", Var "x"), Int -1))))), Var "f"))
    envWithIfAndPlus

// And we can do recursive value bindings too?


// F# checks this

//type Foo = { foo: Foo}
//let rec x = { foo= x }

let r =
    Record
        (Map.ofList
            ([ ("foo", Int 23)
               ("bar", Abs("x", Var("x"))) ]))

typecheckInEnv r Map.empty

let r2 =
    Let
        ("f", Abs("x", Var("x")),
         Record
             (Map.ofList
                 ([ ("foo", Int 23)
                    ("bar", App(Var "f", Int 34)) ])))

typecheckInEnv r2 Map.empty

let r3 =
    Abs
        ("f",
         Record
             (Map.ofList
                 ([ ("a", App(Var "f", Abs("x", Var("x"))))
                    ("b", App(Var "f", Int 34)) ])))

typecheckInEnv r3 Map.empty

let r4 =
    Record
        (Map.ofList
            ([ ("a", App(Var "f", Abs("x", Var("x"))))
               ("b", App(Var "f", Int 34)) ]))

let newTy = newTypeVar()

ti (Map.add "f" (Scheme([], newTy)) Map.empty) r4
