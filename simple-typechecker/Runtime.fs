module Runtime

open Expressions

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
