module Expressions

type Exp =
    | Int of int
    | Var of string
    | Abs of string * Exp // fun x -> x* 2
    | App of Exp * Exp // f 20
    | Let of string * Exp * Exp
    | LetRec of string * Exp * Exp
    | Record of Rec

and Rec = Map<string, Exp>
