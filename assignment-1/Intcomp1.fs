(* Programming language concepts for software developers, 2012-02-17 *)

(* Evaluation, checking, and compilation of object language expressions *)
(* Stack machines for expression evaluation                             *)

(* Object language expressions with variable bindings and nested scope *)

module Intcomp1 =

type expr =
    | CstI of int
    | Var of string
    | Let of (string * expr) list * expr
    | Prim of string * expr * expr

(* Some closed expressions: *)

let e1 = Let(["z", CstI 17], Prim("+", Var "z", Var "z"))

let e2 = Let(["z", CstI 17],
             Prim("+", Let(["z", CstI 22], Prim("*", CstI 100, Var "z")),
                       Var "z"))

let e3 = Let(["z", Prim("-", CstI 5, CstI 4)],
             Prim("*", CstI 100, Var "z"))

let e4 = Prim("+", Prim("+", CstI 20, Let(["z", CstI 17], Prim("+", Var "z", CstI 2))), CstI 30)

let e5 = Prim("*", CstI 2, Let(["x", CstI 3], Prim("+", Var "x", CstI 4)))

let e6 = Let(["z", Var "x"], Prim("+", Var "z", Var "x"))

let e7 = Let(["z", CstI 3],
             Let(["y", Prim("+", Var "z", CstI 1)],
                 Prim("+", Var "z", Var "y")))

let e8 = Let(["z", Let(["x", CstI 4], Prim("+", Var "x", CstI 5))],
             Prim("*", Var "z", CstI 2))

let e9 = Let(["z", CstI 3],
             Let(["y", Prim("+", Var "z", CstI 1)],
                 Prim("+", Var "x", Var "y")))

let e10 = Let(["z", Prim("+", Let(["x", CstI 4], Prim("+", Var "x", CstI 5)), Var "x")],
              Prim("*", Var "z", CstI 2))

(* ---------------------------------------------------------------------- *)

(* Evaluation of expressions with variables and bindings *)

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r -> if x = y then v else lookup r x

let rec eval en (env: (string * int) list) : int =
    match en with
    | CstI i -> i
    | Var x -> lookup env x
    | Let(lst, e) ->
        match lst with
        | (str, ie) :: tail ->
            let updatedEnv = (str, (eval ie env)) :: env
            eval e updatedEnv
        | [] -> eval e env
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim _ -> failwith "unknown primitive"

let run e = eval e []
let res = List.map run [e1; e2; e3; e4; e5; e7;] 

(* ---------------------------------------------------------------------- *)

(* Closedness *)

let rec mem x vs =
    match vs with
    | [] -> false
    | v :: vr -> x = v || mem x vr

(* ---------------------------------------------------------------------- *)

let rec lookOrSelf env x =
    match env with
    | [] -> Var x
    | (y, e) :: r -> if x = y then e else lookOrSelf r x

(* Remove (x, _) from env: *)

let rec remove env x =
    match env with
    | [] -> []
    | (y, e) :: r -> if x = y then r else (y, e) :: remove r x

(* ---------------------------------------------------------------------- *)

(* Free variables *)

(* Operations on sets, represented as lists.  Simple but inefficient;
   one could use binary trees, hashtables or splaytrees for
   efficiency. *)

(* union(xs, ys) is the set of all elements in xs or ys, without duplicates *)

let rec union (xs, ys) =
    match xs with
    | [] -> ys
    | x :: xr -> if mem x ys then union (xr, ys) else x :: union (xr, ys)

(* minus xs ys  is the set of all elements in xs but not in ys *)

let rec minus (xs, ys) =
    match xs with
    | [] -> []
    | x :: xr -> if mem x ys then minus (xr, ys) else x :: minus (xr, ys)

(* Find all variables that occur free in expression e *)

let rec freevars (e: expr) : string list =
    match e with
    | CstI i -> []
    | Var x -> [x]
    | Let(bindings, body) ->
        let bounds = List.map (fun (x, _) -> x) bindings in
        let rVars = List.collect (fun (_, erhs) -> freevars erhs) bindings in
        let bVars = minus (freevars body, bounds) in
        union (rVars, bVars)
    | Prim(ope, e1, e2) -> union (freevars e1, freevars e2)

(* Alternative definition of closed *)

let closed2 e = (freevars e = [])
let res2 = List.map closed2 [ e1; e2; e3; e4; e5; e6; e7; e8; e9; e10; ]

(* ---------------------------------------------------------------------- *)

(* Compilation to target expressions with numerical indexes instead of
   symbolic variable names.  *)

type texpr = (* target expressions *)
    | TCstI of int
    | TVar of int (* index into runtime environment *)
    | TLet of texpr * texpr (* erhs and ebody *)
    | TPrim of string * texpr * texpr


(* Map variable name to variable index at compile-time *)

let rec getindex vs x =
    match vs with
    | [] -> failwith "Variable not found"
    | y :: yr -> if x = y then 0 else 1 + getindex yr x

(* Compiling from expr to texpr *)

let rec tcomp (e: expr) (cenv: string list) : texpr =
    match e with
    | CstI i -> TCstI i
    | Var x -> TVar (getindex cenv x)
    | Let (bindings, ebody) ->
        match bindings with 
        |[x, expr] -> 
            let lvar = x
            let cenv1 = lvar :: cenv
            let erhs = expr
            TLet(tcomp erhs cenv, tcomp ebody cenv1)
    | Prim (ope, e1, e2) -> TPrim (ope, tcomp e1 cenv, tcomp e2 cenv)

let test = tcomp e1 []
let test1 = tcomp e2 []
let test2 = tcomp e3 []
let test3 = tcomp e4 []
let test4 = tcomp e5 []

(* -----------------------------------------------------------------  *)
