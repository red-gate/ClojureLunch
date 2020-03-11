// Abstract syntax for pure functional language
// We'll need abstraction, application, anonymous functions, let bindings

type Exp = 
  | Int of int
  | Var of string
  | Abs of string * Exp  // fun x -> x* 2
  | App of Exp * Exp  // f 20
  | Let of string * Exp * Exp 
  | LetRec of string * Exp * Exp
  | Record of Rec 
and Rec = Map<string, Exp>

type Runtime =
  | RInt of int
  | RAbs of string * Exp * REnv
  | Plus
  | Plus1 of int
  | IF0
  | IFTrue0
  | IFTrue1 of Runtime
  | IFFalse0
  | IFFalse1
  
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

let envPlus = Map.ofList [("+", Plus); ("if", IF0)]

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

// We'll target 

//let a = 
//  let id x = x
//  (id 2, id true)

// but let's start with 

//let a = 
//  let id x = x
//  id 2

// We'll do this the functional way (though this makes some things a lot harder)
//  - typically you'd use mutable slots to represent the substituions that we'll end up doing by rewrites

// Define the type representing types
type Typ =  TVar of string
          | TInt
          | TFun of Typ * Typ
          | TRecord of TRec
and TRec = Map<string, Typ>

// And a means to get new type variables
let private counter = ref 0

let nextId () = 
  let id = !counter
  counter := !counter + 1
  id
  
let newTypeVar () = TVar  (sprintf "%i" (nextId()))
                 
// We'll also need type environments and type schemes

// The type of the identity function
// Scheme (["a"], TFun (TVar "a", TVar "a"))  - id : a-> a
type Scheme = Scheme of string list * Typ

// An environment with the identity function
// Map.ofList [("id", Scheme (["a"], TFun (TVar "a", TVar "a")))]
type TypeEnv = Map<string, Scheme>

type Subst = Map<string, Typ>

// The basic technique requires unification and substitution so let's write those on some simple examples
//  - what's unification (and the most general unifier)
//  - how do we apply a subsitution to a type/type scheme/type environment
//  - and compose substituions

let rec applySub (m : Map<string, Typ>) (t : Typ) : Typ = 
  match t with
    | TVar tname -> match Map.tryFind tname m with
                      | Some x -> x
                      | None -> t
    | TFun (x,y) -> TFun(applySub m x, applySub m y) 
    | TRecord m' -> Map.map(z, x => applySub m x), m')
    | _ -> t

let applySubToScheme subst (Scheme(vars, body)) =
  let newSub = List.foldBack Map.remove vars subst
  Scheme (vars, applySub newSub body )

let applySubToEnv (m : Subst) (e : TypeEnv) : TypeEnv =
  Map.map (fun _ v -> applySubToScheme m v) e


let rec occurs v exp =
  match exp with
  | TVar t -> v = t
  | TInt -> false
  | TFun(x,y) -> (occurs v x) || (occurs v y)


let rec unify ty1 ty2 =
  match ty1, ty2 with
  | TInt, TInt -> Map.empty
  | TVar s, TInt -> Map.add s TInt Map.empty
  | TInt, TVar s -> Map.add s TInt Map.empty
  | TVar s, TVar t when s=t -> Map.empty
  | TVar s, TVar t -> Map.add s ty2 Map.empty
  | TFun (a,b), TFun (c,d) -> 
      let s = unify a c 
      let b1 = applySub s b 
      let d1 = applySub s d
      let s2 = unify b1 d1
      let x = Map.foldBack (fun k v -> Map.add k (applySub s2 v)) s Map.empty
      Map.foldBack Map.add x s2
  | TVar s, TFun _ when not (occurs s ty2) ->
      Map.add s ty2 Map.empty
  | TFun _, TVar s when not (occurs s ty1) ->
      Map.add s ty1 Map.empty
  | _, _ -> printf "%A %A" ty1 ty2
            failwith "Incompatible types"

  // TFun _ _ , TInt -> // fail 

let subst = unify (TFun(TVar("a"), TVar("b"))) (TFun(TVar("b"), TInt))

//let sybst2 = unify (TVar "a") (TFun ((TVar "a"), (TVar "b")))

// instantiate to get fresh variables when we look something up
// We can now put the identity in the type environment and look it up

let instantiate (s : Scheme) =
  match s with
  | Scheme (vars, typ) -> 
    let subst = vars |> List.map (fun x -> (x, newTypeVar()) )
                |> Map.ofList
    applySub subst typ

let inst1 = instantiate (Scheme (["a"], TFun (TVar "a", TVar "a")) )

// generalise to make a schema when we let bind something
let rec typeVariables t = 
 match t with 
 | TVar s -> Set.singleton s
 | TInt -> Set.empty
 | TFun (t1, t2) -> Set.union (typeVariables t1) (typeVariables t2)
 
let schemeFreeVariables (s : Scheme )  = 
  match s with 
  | Scheme (vars, typ) -> Set.difference (typeVariables typ) (Set.ofList vars)

let freeTypeVariables (e : TypeEnv) =
  Seq.foldBack (fun (KeyValue(_, scheme)) state -> Set.union (schemeFreeVariables scheme) state) e Set.empty

let generalise env t =
  let fvEnv = freeTypeVariables env
  let fvT = typeVariables t
  Scheme(List.ofSeq (Set.difference fvT fvEnv), t)

// We can now generalise the identity function

let env = Map.ofList([ ("f", Scheme(["a"], TFun(TVar "a", TVar "b")))])

let res = generalise env (TFun(TVar "a", TVar "b")) 

let composeSubstition outer inner = 
  Map.foldBack (fun k v state -> Map.add k (applySub outer v) state) inner outer // handcrafted union

// Now we just put it all together

// The typecheck worker function will return a type and a substitution

// Let's try

//let a = fun x -> x // Abstract

//let a : int -> int = (fun x -> x) (fun x -> x) //Apply

let rec ti (env : TypeEnv) (exp : Exp) : Subst * Typ =
  match exp with 
  | Int n -> (Map.empty, TInt)
  | Var x -> match Map.tryFind x env with
              | None -> failwith (sprintf "nope %A" x)
              | Some s -> (Map.empty, instantiate s)
  | Abs(x,e) -> 
       let envWithoutX = Map.remove x env
       let newTv = newTypeVar()
       let (subst, Tbody) = ti (Map.add x (Scheme([], newTv)) envWithoutX) e
       let newSubst = match newTv with 
                        | TVar newTvName -> (Map.remove newTvName subst)
       let functionType = TFun(applySub subst newTv, Tbody)
       newSubst, functionType
  | Let(x, boundExpr, body) -> 
      let (s1, t1) = ti env boundExpr
      let scheme1 = generalise (applySubToEnv s1 env) t1
      let cleanEnv = Map.remove x env
      let newEnv = Map.add x scheme1 cleanEnv
      let (s2, t2) = ti (applySubToEnv s1 newEnv) body
      (composeSubstition s2 s1,t2)
  | App(e1, e2) -> 
      let (s1, t1) = ti env e1
      let (s2, t2) = ti (applySubToEnv s1 env) e2
      let tv = newTypeVar ()
      let s3 = unify (applySub s2 t1) (TFun(t2,tv))
      (composeSubstition s3 (composeSubstition s2 s1), applySub s3 tv)
  | LetRec(x, boundExpr, body) ->   
      let tv = newTypeVar ()
      let newEnv = Map.add x (Scheme([],tv)) env
      let (s1, t1) = ti newEnv boundExpr
      let scheme1 = generalise (applySubToEnv s1 env) t1
      let cleanEnv = Map.remove x env
      let newEnv' = Map.add x scheme1 cleanEnv
      let (s2, t2) = ti (applySubToEnv s1 newEnv') body
      (composeSubstition s2 s1,t2)

let typecheckInEnv exp env = 
  snd (ti env exp)

let envWithid = Map.ofList([ ("id", Scheme(["a"], TFun(TVar "a", TVar "a")))])

typecheckInEnv (Var "id") envWithid
typecheckInEnv (Abs("x",Var "x")) Map.empty
typecheckInEnv (Abs("x",Int 2)) Map.empty
typecheckInEnv (Let("id",Abs ("x", Var "x"), Var "id" )) Map.empty
typecheckInEnv (Let("id",Abs ("x", Var "x"), App (Var "id", Int 3) )) Map.empty
typecheckInEnv (Let("id",Abs ("x", Var "x"),(App(App(Abs("p", Abs("q", Var("q"))),App(Var "id", Int 3)),Var "id" )))) Map.empty

// Type check a recursive function

//let rec f = fun x -> if x = 0 then 0 else 1 + f(x-1)

//let rec bottom = fun x -> bottom x

let envWithIfAndPlus = 
  Map.ofList([ 
    ("+", Scheme([], TFun(TInt, TFun(TInt, TInt)))); 
    ("if", Scheme(["a"], TFun(TInt, TFun(TVar "a", TFun(TVar "a", TVar "a")))) )
   ])

typecheckInEnv (LetRec("x",Abs ("x", Var "x"), Var "x" )) envWithIfAndPlus
typecheckInEnv (LetRec("bottom",Abs ("x", App(Var "bottom", Var "x")), Var "bottom" )) envWithIfAndPlus
typecheckInEnv (LetRec("f", 
  Abs("x", App(App(App(Var "if", Var "x"), Int 0), 
    App(App(Var "+", Int 1), 
      App(Var "f", 
        App(App(Var "+", Var "x"), Int -1)
      )
    )
  )), Var "f")) envWithIfAndPlus

// And we can do recursive value bindings too?


// F# checks this

//type Foo = { foo: Foo}
//let rec x = { foo= x }
let r = Record (Map.ofList([ ("foo", Var "x") ]))
typecheckInEnv (LetRec("x", r, (Var "x"))) envWithIfAndPlus
