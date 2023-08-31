(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

(* Association lists map object language variables to their values *)

let env =
    [ ("a", 3)
      ("c", 78)
      ("baf", 666)
      ("b", 111)
      ("v", 23)
      ("w", 55)
      ("z", 27)
      ("x", 99)
      ("y", 42) ]

let emptyenv = [] (* the empty environment *)

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r -> if x = y then v else lookup r x

let cvalue = lookup env "c"


(* Object language expressions with variables *)

type expr =
    | CstI of int
    | Var of string
    | Prim of string * expr * expr
    | If of expr * expr * expr

let rec eval e (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x -> lookup env x
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env

        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "==" -> if i1 = i2 then 1 else 0
        | "max" -> if i1 > i2 then i1 else i2
        | "min" -> if i1 < i2 then i1 else i2
        | _ -> failwith "unknown primitive"
    | If(e1, e2, e3) -> if (eval e1 env) <> 0 then eval e2 env else eval e3 env

let e1 = CstI 17
let e2 = Prim("+", CstI 3, Var "a")
let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a")
let equals_false = Prim("==", Var "b", CstI 20) // should eval to 0
let equals_true = Prim("==", Var "b", CstI 111) // should eval to 1
let max_first = Prim("max", Var "baf", CstI 665) // should eval to 666
let max_second = Prim("max", Var "baf", CstI 667) // should eval to 667
let min_first = Prim("min", Var "b", Var "baf") // Should eval to 111
let min_second = Prim("min", Var "a", CstI 1) // Should eval to 1
let if_true = If(Var "a", CstI 11, CstI 22) // should eval to 11
let if_false = If(CstI 0, CstI 11, CstI 22) // should eval to 22
let e1v = eval e1 env
let e2v1 = eval e2 env
let e2v2 = eval e2 [ ("a", 314) ]
let e3v = eval e3 env

let e4v = eval equals_false env
let e4_1v = eval equals_true env

let e5v = eval max_first env
let e5_1 = eval max_second env

let e6v = eval min_first env

let e6_1 = eval min_second env

let e7 = eval if_true env

let e7_1 = eval if_false env

type aexpr =
    | CstI of int
    | Var of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr


let rec aeval e (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x -> lookup env x
    | Add(ae1, ae2) -> (aeval ae1 env) + (aeval ae2 env)
    | Mul(ae1, ae2) -> (aeval ae1 env) * (aeval ae2 env)
    | Sub(ae1, ae2) -> (aeval ae1 env) - (aeval ae2 env)

let ae1v = aeval (Sub(Var "v", Add(Var "w", Var "z"))) env // v - (w+z)
let ae2v = aeval (Mul(CstI 2, (Sub(Var "v", Add(Var "w", Var "z"))))) env // 2*(v-(w+z))
let ae3v = aeval (Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))) env // x + y + z + v
