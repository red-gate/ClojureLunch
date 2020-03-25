module Typechecker

open Expressions

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
type Typ =
    | TVar of string
    | TInt
    | TFun of Typ * Typ
    | T of TRec

and TRec = Map<string, Typ>

// And a means to get new type variables
let private counter = ref 0

let nextId() =
    let id = !counter
    counter := !counter + 1
    id

let newTypeVar() = TVar(sprintf "%i" (nextId()))

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

let rec applySub (m: Map<string, Typ>) (t: Typ): Typ =
    match t with
    | TVar tname ->
        match Map.tryFind tname m with
        | Some x -> x
        | None -> t
    | TFun(x, y) -> TFun(applySub m x, applySub m y)
    | T m' -> T(Map.map (fun _ x -> applySub m x) m')
    | _ -> t

let applySubToScheme subst (Scheme(vars, body)) =
    let newSub = List.foldBack Map.remove vars subst
    Scheme(vars, applySub newSub body)

let applySubToEnv (m: Subst) (e: TypeEnv): TypeEnv = Map.map (fun _ v -> applySubToScheme m v) e


let rec occurs v exp =
    match exp with
    | TVar t -> v = t
    | TInt -> false
    | TFun(x, y) -> (occurs v x) || (occurs v y)


let rec unify ty1 ty2 =
    match ty1, ty2 with
    | TInt, TInt -> Map.empty
    | TVar s, TInt -> Map.add s TInt Map.empty
    | TInt, TVar s -> Map.add s TInt Map.empty
    | TVar s, TVar t when s = t -> Map.empty
    | TVar s, TVar t -> Map.add s ty2 Map.empty
    | TFun(a, b), TFun(c, d) ->
        let s = unify a c
        let b1 = applySub s b
        let d1 = applySub s d
        let s2 = unify b1 d1
        let x = Map.foldBack (fun k v -> Map.add k (applySub s2 v)) s Map.empty
        Map.foldBack Map.add x s2
    | TVar s, TFun _ when not (occurs s ty2) -> Map.add s ty2 Map.empty
    | TFun _, TVar s when not (occurs s ty1) -> Map.add s ty1 Map.empty
    | _, _ ->
        printf "%A %A" ty1 ty2
        failwith "Incompatible types"

// TFun _ _ , TInt -> // fail

let subst = unify (TFun(TVar("a"), TVar("b"))) (TFun(TVar("b"), TInt))

//let sybst2 = unify (TVar "a") (TFun ((TVar "a"), (TVar "b")))

// instantiate to get fresh variables when we look something up
// We can now put the identity in the type environment and look it up

let instantiate (s: Scheme) =
    match s with
    | Scheme(vars, typ) ->
        let subst =
            vars
            |> List.map (fun x -> (x, newTypeVar()))
            |> Map.ofList
        applySub subst typ

let inst1 = instantiate (Scheme([ "a" ], TFun(TVar "a", TVar "a")))

// generalise to make a schema when we let bind something
let rec typeVariables t =
    match t with
    | TVar s -> Set.singleton s
    | TInt -> Set.empty
    | TFun(t1, t2) -> Set.union (typeVariables t1) (typeVariables t2)

let schemeFreeVariables (s: Scheme) =
    match s with
    | Scheme(vars, typ) -> Set.difference (typeVariables typ) (Set.ofList vars)

let freeTypeVariables (e: TypeEnv) =
    Seq.foldBack (fun (KeyValue(_, scheme)) state -> Set.union (schemeFreeVariables scheme) state) e Set.empty

let generalise env t =
    let fvEnv = freeTypeVariables env
    let fvT = typeVariables t
    Scheme(List.ofSeq (Set.difference fvT fvEnv), t)

// We can now generalise the identity function

let env = Map.ofList ([ ("f", Scheme([ "a" ], TFun(TVar "a", TVar "b"))) ])

let res = generalise env (TFun(TVar "a", TVar "b"))

let composeSubstition outer inner =
    Map.foldBack (fun k v state -> Map.add k (applySub outer v) state) inner outer // handcrafted union

// Now we just put it all together

// The typecheck worker function will return a type and a substitution

// Let's try

//let a = fun x -> x // Abstract

//let a : int -> int = (fun x -> x) (fun x -> x) //Apply

let rec ti (env: TypeEnv) (exp: Exp): Subst * Typ =
    printf "Typechecinkg: %A" exp
    match exp with
    | Int n -> (Map.empty, TInt)
    | Var x ->
        match Map.tryFind x env with
        | None -> failwith (sprintf "nope %A" x)
        | Some s -> (Map.empty, instantiate s)
    | Abs(x, e) ->
        let envWithoutX = Map.remove x env
        let newTv = newTypeVar()
        let (subst, Tbody) = ti (Map.add x (Scheme([], newTv)) envWithoutX) e

        let newSubst =
            match newTv with
            | TVar newTvName -> (Map.remove newTvName subst)

        let functionType = TFun(applySub subst newTv, Tbody)
        newSubst, functionType
    | Let(x, boundExpr, body) ->
        let (s1, t1) = ti env boundExpr
        let scheme1 = generalise (applySubToEnv s1 env) t1
        let cleanEnv = Map.remove x env
        let newEnv = Map.add x scheme1 cleanEnv
        let (s2, t2) = ti (applySubToEnv s1 newEnv) body
        (composeSubstition s2 s1, t2)
    | App(e1, e2) ->
        let (s1, t1) = ti env e1
        let (s2, t2) = ti (applySubToEnv s1 env) e2
        let tv = newTypeVar()
        let s3 = unify (applySub s2 t1) (TFun(t2, tv))
        (composeSubstition s3 (composeSubstition s2 s1), applySub s3 tv)
    | LetRec(x, boundExpr, body) ->
        let tv = newTypeVar()
        let newEnv = Map.add x (Scheme([], tv)) env
        let (s1, t1) = ti newEnv boundExpr
        let scheme1 = generalise (applySubToEnv s1 env) t1
        let cleanEnv = Map.remove x env
        let newEnv' = Map.add x scheme1 cleanEnv
        let (s2, t2) = ti (applySubToEnv s1 newEnv') body
        (composeSubstition s2 s1, t2)
    | Record(r) ->
        Map.foldBack (fun k v (subst, (T mapTypes)) ->
            let (s, t) = ti env v
            (composeSubstition s subst, T(Map.add k t mapTypes))) r (Map.empty, T Map.empty) // ( substituitions,  labels->types) -- fixme
    | _ -> failwithf "%A" exp

// > Record "x" -> 2 * 3, "y" -> fun x -> x
//     > ti 2*3
//     < TInt, s1
//     > ti fun x -> x
//     < TFun(..), s2
// <  s1 combined with s2,   T "x" -> TInt "y" -> TFun(..)

let typecheckInEnv exp env = snd (ti env exp)
