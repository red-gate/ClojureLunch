module Examples

open Expressions

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

// if 0 then 1 else bang

let ex8 = App(App(App(Var("if"), Int(0)), Int(1)), Var("bang"))

// if 3 then 1 else bang

let ex9 = App(App(App(Var("if"), Int(1)), Var("bang")), Int(2))

// Z (fun triang' -> fun n -> if n = 0 then 1 else n + triang' (n+-1)) 3

let Z =
  Abs("f",
    App(Abs("x", App(Var("f"), Abs("v", App(App(Var("x"), Var("x")), Var("v"))))),
        Abs("x", App(Var("f"), Abs("v", App(App(Var("x"), Var("x")), Var("v")))))))

let ex10 =
  Let("Z", Z, 
          App(
            App(Var("Z"),
              Abs("triang'",
                Abs("n",
                  App(App(App(Var("if"), Var("n")), Int 0),
                            App(App(Var("+"), Var("n")), App(Var("triang'"), App(App(Var("+"), Var("n")), Int -1))))))),
            Int 3))

