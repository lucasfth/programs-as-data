# programs-as-data

## Created by

fefa and luha

## Table of contents

* [Assignment 1](#assignment-1)
* [Assignment 2](#assignment-2)
  * [Exercise 2.4 from PLC](#exercise-24-from-plc)
  * [Exercise 2.5 from PLC](#exercise-25-from-plc)
  * [Exercise 3.2 from PLC](#exercise-32-from-plc)
  * [Exercise 2.1 from BCD](#exercise-21-from-bcd)
  * [Exercise 2.2 from BCD](#exercise-22-from-bcd)
* [Assignment 3](#assignment-3)
  * [Exercise 3.3](#exercise-33)
  * [Exercise 3.4](#exercise-34)
  * [Exercise 3.5](#exercise-35)
  * [Exercise 3.6](#exercise-36)
  * [Exercise 3.7](#exercise-37)
* [Assignment 4](#assignment-4)
  * [Exercise 4.1](#exercise-41)
  * [Exercise 4.2](#exercise-42)
  * [Exercise 4.3](#exercise-43)
  * [Exercise 4.4](#exercise-44)
  * [Exercise 4.5](#exercise-45)
* [Assignment 5](#assignment-5)
  * [Exercise 5.1](#exercise-51)
  * [Exercise 5.7](#exercise-57)
  * [Exercise 6.1](#exercise-61)
  * [Exercise 6.2](#exercise-62)
  * [Exercise 6.3](#exercise-63)
  * [Exercise 6.4](#exercise-64)
  * [Exercise 6.5](#exercise-65)
* [Assignment 6](#assignment-6)
  * [Exercise 7.1](#exercise-71)
  * [Exercise 7.2](#exercise-72)
  * [Exercise 7.3](#exercise-73)
  * [Exercise 7.4](#exercise-74)
  * [Exercise 7.5](#exercise-75)

## Assignment 1

Only changed Intcomp1.fs and Intro2.fs.
Also created the file SimpleExpr.java

## Assignment 2

### Exercise 2.4 from PLC

Changed three funtions in Intcomp.fs

```fsharp
let rec scomp (e : expr) (cenv : stackvalue list) : sinstr list =
    match e with
    | CstI i -> [SCstI i]
    | Var x  -> [SVar (getindex cenv (Bound x))]
    | Let (tup, ebody) ->
        let rec bindings b sinlst cenv' =
            match b with
            | [] -> sinlst, cenv'
            | (str, exp) :: tail ->
                bindings tail (scomp exp cenv' @ sinlst) (Bound str :: cenv')
        
        let sinstrs, newcenv = bindings tup [] cenv
        let sinstrs2 = scomp ebody newcenv
        sinstrs @ sinstrs2 @ [SSwap; SPop]
    | Prim("+", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SAdd] 
    | Prim("-", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SSub] 
    | Prim("*", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SMul] 
    | Prim _ -> failwith "scomp: unknown operator";;

let sinstrToInt sn =
    match sn with
    | SCstI n -> [0; n]
    | SVar n -> [1; n]
    | SAdd -> [2]
    | SSub -> [3]
    | SMul -> [4]
    | SPop -> [5]
    | SSwap -> [6]

let rec assemble (sn: sinstr list) =
    let rec aux lst acc =
        match lst with
        | x :: tail -> aux tail (acc @ (sinstrToInt x))
        | _ -> acc
    aux sn []
```

### Exercise 2.5 from PLC

Changed Machine.java

```java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

class Machine {

  final static int 
    SCST = 0, SVAR = 1, SADD = 2, SSUB = 3, SMUL = 4, SPOP = 5, SSWAP = 6;
  
  public static void main(String[] args) {
      ArrayList<Integer> listOfint = new ArrayList<Integer>();
      Scanner scanner;
      try {
            scanner = new Scanner(new File("../"+args[0]));
            while(scanner.hasNext()){
                  listOfint.add(scanner.nextInt());
            }
            int[] rpn3 = listOfint.stream().mapToInt(i -> i).toArray();
            System.out.println(seval(rpn3));
      } catch (FileNotFoundException e) {
            e.printStackTrace();
      }
  }
  
  static int seval(int[] code) {
    int[] stack = new int[1000]; // evaluation and env stack
    int sp = -1; // pointer to current stack top

    int pc = 0; // program counter
    int instr; // current instruction

    while (pc < code.length) 
      switch (instr = code[pc++]) {
      case SCST:
        stack[sp+1] = code[pc++]; sp++; break;
      case SVAR:
        stack[sp+1] = stack[sp-code[pc++]]; sp++; break;
      case SADD: 
        stack[sp-1] = stack[sp-1] + stack[sp]; sp--; break;
      case SSUB: 
        stack[sp-1] = stack[sp-1] - stack[sp]; sp--; break;
      case SMUL: 
        stack[sp-1] = stack[sp-1] * stack[sp]; sp--; break;
      case SPOP: 
        sp--; break;
      case SSWAP: 
        { 
            int tmp     = stack[sp]; 
            stack[sp]   = stack[sp-1]; 
            stack[sp-1] = tmp;
            break;
        }
      default:
        throw new RuntimeException("Illegal instruction " + instr 
            + " at address " + (pc-1));
      }
    return stack[sp];      
  }
}
```

### Exercise 3.2 from PLC

The regex for matching on a string containing `a` and `b` where there had to be an `b` between all `a`'s, could be:

```regex
^a?((b+)(a?))*$
```

NFA diagram

```mermaid
flowchart TD

ids((start))
id1((1))
id2((2))
id3((3))
ide(((end)))

ids -- ε --> id1
id1 -- a --> id2
id1 -- b --> id3
id2 -- ε --> ide
id2 -- b --> id3
id3 -- ε --> ide
id3 -- ε --> id1
```

DFA diagram

```mermaid
flowchart TD

ids((start))
id1(((1)))
id2(((2)))
ids -- a --> id1
ids -- b --> id2
id1 -- b --> id2
id2 -- a --> id1
id2 -- b --> id2
```

### Exercise 2.1 from BCD

a. `^(0)*42$`
b. `^(?!0*42$)\d+$`
c. `^(0)*([4][3-9]|[5-9]\d|[1-9]\d{2,})$`

### Exercise 2.2 from BCD

Given the regex `a*(a|b)aa` we constructed the given NFA and DFA.

NFA

```regex
flowchart TD

ids((start))
id1((1))
id2((2))
id3((3))
id4((4))
id5((5))
ide(((end)))

ids -- ε --> id1
id1 -- a --> id2
id2 -- ε --> id1
id1 -- a --> id3
id1 -- b --> id3
id3 -- a --> id4
id4 -- a --> id5
id5 -- ε --> ide
```

DFA

```regex
flowchart TD

ids((start))
id1((1))
id2((2))
id3((3))
id4((4))
id5(((5)))
ide(((end)))

ids -- a --> id1
ids -- b --> id2
id1 -- a --> id3
id1 -- b --> id2
id2 -- a --> id4
id3 -- a --> id5
id3 -- b --> id2
id4 -- a --> ide
id5 -- a --> id5
id5 -- b --> id2
```

### hellolex

#### Question 1

What are the regular expressions involved, and which semantic values are they associated with?

The regex is `['0'-'9']` and can match a single  number between 0 and 9 inclusive.

#### Question 2

Generate the lexer out of the specification using a command prompt. Which additional file is generated during the process?

How many states are there by the automaton of the lexer? Hint: Depending on setup, you can generate the lexer with the command fslex --unicode hello.fsl from the command prompt. You can get the number of the states of the automaton by reading the report output when the lexer is generated.

## Assignment 3

### Exercise 3.3

Rightmost deriviation given `let z = (17) in z + 2 * 3 end EOF`

| rule | match |
| --- | --- |
| A | **Expr** EOF |
| F | LET NAME EQ Expr IN **Expr** END EOF |
| H | LET NAME EQ Expr IN Expr PLUS **Expr** END EOF |
| G | LET NAME EQ Expr IN Expr PLUS Expr TIMES **Expr** END EOF |
| C | LET NAME EQ Expr IN Expr PLUS **Expr** TIMES 3 END EOF |
| C | LET NAME EQ Expr IN **Expr** PLUS 2 TIMES 3 END EOF |
| B | LET NAME EQ **Expr** IN z PLUS 2 TIMES 3 END EOF |
| E | LET NAME EQ **(Expr)** IN z PLUS 2 TIMES 3 END EOF |
| C | LET NAME EQ 17 IN z PLUS 2 TIMES 3 END EOF |
| B | LET z EQ 17 IN z PLUS 2 TIMES 3 END EOF |

### Exercise 3.4

```mermaid
flowchart TD

main --> Expr
main --> EOF
Expr --> LET
Expr --> NAME
Expr --> EQ
NAME --> z
eq[=]
EQ --> eq
Expr --> LPAR
lpar["("]
LPAR --> lpar
expr1[Expr]
Expr --> expr1
expr1 --> CSTINT
CSTINT --> 17
Expr --> RPAR
rpar[")"]
RPAR --> rpar
Expr --> in
expr2[Expr]
Expr --> expr2
name1[NAME]
expr2 --> name1
z1[z]
name1 --> z1
expr2 --> PLUS
expr3[Expr]
PLUS --> +
expr2 --> expr3
cstint1[CSTINT]
expr3 --> cstint1
cstint1 --> 2
expr3 --> TIMES
TIMES --> *
cstint2[CSTINT]
expr3 --> cstint2
cstint2 --> 3
Expr --> END

```

### Exercise 3.5

![screenshot](./screenshots/a3_e3_5.png)

<details>

<summary> Commands to compile and run </summary>

```bash
mono ~/bin/fsharp/fslex.exe --unicode ExprLex.fsl
mono ~/bin/fsharp/fsyacc.exe --module ExprPar ExprPar.fsy
fsharpi -r ~/bin/fsharp/FsLexYacc.Runtime.dll Absyn.fs ExprPar.fs ExprLex.fs Parse.fs
```

</details>

### Exercise 3.6

Added the `compString` function to `Expr.fs`. The code can be seen in the file and also below:

```fsharp
let compString (str : string) : sinstr list =
    let e : expr = fromString str
    scomp e []
```

### Exercise 3.7

Changed following type in `Absyn.fs`

```fsharp
type expr = 
  | CstI of int
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
```

Changed following function in `Expr.fs`

```fsharp
let rec scomp e (cenv : rtvalue list) : sinstr list =
    match e with
      | CstI i -> [SCstI i]
      | Var x  -> [SVar (getindex cenv (Bound x))]
      | Let(x, erhs, ebody) -> 
            scomp erhs cenv @ scomp ebody (Bound x :: cenv) @ [SSwap; SPop]
      | If(cond, l1, l2) ->
            let el1 = scomp l1 cenv
            let el2 = scomp l2 cenv
            scomp cond cenv @ [SIf(el1, el2)]
      | Prim("+", e1, e2) -> 
            scomp e1 cenv @ scomp e2 (Intrm :: cenv) @ [SAdd] 
      | Prim("-", e1, e2) -> 
            scomp e1 cenv @ scomp e2 (Intrm :: cenv) @ [SSub] 
      | Prim("*", e1, e2) -> 
            scomp e1 cenv @ scomp e2 (Intrm :: cenv) @ [SMul] 
      | Prim _ -> raise (Failure "scomp: unknown operator")
```

Changed following type in `Expr.fs`

```fsharp
type sinstr =
  | SCstI of int                        (* push integer           *)
  | SVar of int                         (* push variable from env *)
  | SAdd                                (* pop args, push sum     *)
  | SSub                                (* pop args, push diff.   *)
  | SMul                                (* pop args, push product *)
  | SPop                                (* pop value/unbind var   *)
  | SIf of sinstr list * sinstr list    (* pop test, then/else    *)
  | SSwap                               (* exchange top and next  *)
```

Changed following keyword in `ExprLex.fsl`

```fsharp
let keyword s =
    match s with
    | "let" -> LET
    | "in"  -> IN
    | "if" -> IF
    | "end" -> END
    | _     -> NAME s
```

Added following token in `ExprPar.fsy`

```fsharp
%token IF
```

Changed following Expr in `ExprPar.fsy`

```fsharp
Expr:
    NAME                                { Var $1            }
  | CSTINT                              { CstI $1           }
  | MINUS CSTINT                        { CstI (- $2)       }
  | LPAR Expr RPAR                      { $2                }
  | LET NAME EQ Expr IN Expr END        { Let($2, $4, $6)   }
  | IF Expr Expr Expr                   { If($2, $3, $4)    }
  | Expr TIMES Expr                     { Prim("*", $1, $3) }
  | Expr PLUS  Expr                     { Prim("+", $1, $3) }  
  | Expr MINUS Expr                     { Prim("-", $1, $3) } 
```

## Assignment 4

### Exercise 4.1

![screenshot](./screenshots/a4_e4_1.png)

<details>

<summary> Commands to compile and run </summary>

</details>

```bash
~/bin/fsharp/fsyacc --module FunPar FunPar.fsy
~/bin/fsharp/fslex --unicode FunLex.fsl
fsharpi -r ~/bin/fsharp/FsLexYacc.Runtime.dll Absyn.fs FunPar.fs FunLex.fs Parse.fs

~/bin/fsharp/fsyacc --module FunPar FunPar.fsy
~/bin/fsharp/fslex --unicode FunLex.fsl
fsharpi -r ~/bin/fsharp/FsLexYacc.Runtime.dll Absyn.fs FunPar.fs FunLex.fs Parse.fs Fun.fs ParseAndRun.fs
```

### Exercise 4.2

Given:

```fsharp
let sum = fromString "let sum n = if n < 1 then 0 else n + (sum (n - 1)) in sum 1000 end";;

run sum;;
```

Computes to:

![screenshot](./screenshots/a4_e4_2_1.png)

Given:

```fsharp
let threeToTheEight = fromString "let power n = if n < 1 then 1 else 3 * (power (n - 1)) in power 8 end";;

run threeToTheEight;;
```

Computes to:

![screenshot](./screenshots/a4_e4_2_2.png)

Given:

```fsharp
let exponentIncrease = fromString @"let power x = if x < 1 then 1 else 3 * (power (x - 1)) in let pow1 n = if n < 12 then power n + pow1 (n + 1) else 0 in pow1 0 end end";;

run exponentIncrease;;
```

Computes to:

![screenshot](./screenshots/a4_e4_2_3.png)

Given:

```fsharp
let baseIncrease = fromString @"let power x = x*x*x*x*x*x*x*x in let pow1 n = if n < 11 then power n + pow1 (n + 1) else 0 in pow1 0 end end";;

run baseIncrease;;
```

Computes to:

![screenshot](./screenshots/a4_e4_2_4.png)

### Exercise 4.3

In Fun.fs the type `value` and the `eval` function was modified to look like this:

```fsharp
type value = 
  | Int of int
  | Closure of string * string list * expr * value env       (* (f, x, fBody, fDeclEnv) *)

let rec eval (e : expr) (env : value env) : int =
    match e with 
    | CstI i -> i
    | CstB b -> if b then 1 else 0
    | Var x  ->
      match lookup env x with
      | Int i -> i 
      | _     -> failwith "eval Var"
    | Prim(ope, e1, e2) -> 
      let i1 = eval e1 env
      let i2 = eval e2 env
      match ope with
      | "*" -> i1 * i2
      | "+" -> i1 + i2
      | "-" -> i1 - i2
      | "=" -> if i1 = i2 then 1 else 0
      | "<" -> if i1 < i2 then 1 else 0
      | _   -> failwith ("unknown primitive " + ope)
    | Let(x, eRhs, letBody) -> 
      let xVal = Int(eval eRhs env)
      let bodyEnv = (x, xVal) :: env
      eval letBody bodyEnv
    | If(e1, e2, e3) -> 
      let b = eval e1 env
      if b<>0 then eval e2 env
      else eval e3 env
    | Letfun(f, x, fBody, letBody) -> 
      let bodyEnv = (f, Closure(f, x, fBody, env)) :: env 
      eval letBody bodyEnv
    | Call(Var f, eArgList) ->
      let fClosure = lookup env f
      match fClosure with
      | Closure (f, xList, fBody, fDeclEnv) -> 
        let vals = List.map2 (fun x y -> (x, Int(eval y env))) xList eArgList 
        let fBodyEnv = vals @ (f, fClosure) :: fDeclEnv 
        eval fBody fBodyEnv
      | _ -> failwith "eval Call: not a function"
    | Call _ -> failwith "eval Call: not first-order function"
```

In Absyn.fs the type `expr` was modified to the following:

```fsharp
type expr = 
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfun of string * string list * expr * expr    (* (f, x, fBody, letBody) *)
  | Call of expr * expr list
```

In FunPar.fsy the follwing expressions where modified to:

```fsharp
AtExpr:
    Const                               { $1                     }
  | NAME                                { Var $1                 }
  | LET NAME EQ Expr IN Expr END        { Let($2, $4, $6)        }
  | LET NAME List EQ Expr IN Expr END   { Letfun($2, $3, $5, $7) }
  | LPAR Expr RPAR                      { $2                     }
;

List:
    NAME                                { [$1]                    }
  | NAME List                           { $1 :: $2                }
;

ArgsList:
  AtExpr                                { [$1]                   }
| AtExpr ArgsList                       { $1 :: $2               }
;

AppExpr:
  | AtExpr ArgsList                     { Call($1, $2)           }
;
```

And added following types

```fsharp
%type <string list> List
%type <expr list> ArgsList
```

### Exercise 4.4

Proof of being able to run the functions:

![screenshot](./screenshots/a4_e4_4.png)

### Exercise 4.5

In FunLex.fsl the rule "&&" and "||" were added

```fsharp
rule Token = parse
  | [' ' '\t' '\r'] { Token lexbuf }
  | '\n'            { lexbuf.EndPos <- lexbuf.EndPos.NextLine; Token lexbuf }
  | ['0'-'9']+      { CSTINT (System.Int32.Parse (lexemeAsString lexbuf)) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*
                    { keyword (lexemeAsString lexbuf) }
  | "(*"            { commentStart := lexbuf.StartPos;
                      commentDepth := 1; 
                      SkipComment lexbuf; Token lexbuf }
  | '='             { EQ }
  | "<>"            { NE }
  | '>'             { GT }
  | '<'             { LT }
  | ">="            { GE }
  | "<="            { LE }
  | '+'             { PLUS }                     
  | '-'             { MINUS }                     
  | '*'             { TIMES }                     
  | '/'             { DIV }                     
  | '%'             { MOD }
  | '('             { LPAR }
  | ')'             { RPAR }
  | "&&"            { AND } 
  | "||"            { OR  }
  | eof             { EOF }
  | _               { failwith "Lexer error: illegal symbol" }
```

In FunPar.fsy the following tokens and Expr were added for AND & OR

```fsharp
%token ELSE END FALSE IF IN LET NOT THEN TRUE
%token PLUS MINUS TIMES DIV MOD
%token EQ NE GT LT GE LE
%token LPAR RPAR
%token AND
%token OR
%token EOF
```

And added precendence for OR & AND

```fsharp
%left OR                /* lowest precedence  */
%left AND
%left ELSE             
%left EQ NE 
%left GT LT GE LE
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT           /* highest precedence  */
```

```fsharp
Expr:
    AtExpr                              { $1                     }
  | AppExpr                             { $1                     }
  | IF Expr THEN Expr ELSE Expr         { If($2, $4, $6)         }
  | MINUS Expr                          { Prim("-", CstI 0, $2)  }
  | Expr PLUS  Expr                     { Prim("+",  $1, $3)     }
  | Expr MINUS Expr                     { Prim("-",  $1, $3)     }
  | Expr TIMES Expr                     { Prim("*",  $1, $3)     }
  | Expr DIV   Expr                     { Prim("/",  $1, $3)     } 
  | Expr MOD   Expr                     { Prim("%",  $1, $3)     }
  | Expr EQ    Expr                     { Prim("=",  $1, $3)     }
  | Expr NE    Expr                     { Prim("<>", $1, $3)     }
  | Expr GT    Expr                     { Prim(">",  $1, $3)     }
  | Expr LT    Expr                     { Prim("<",  $1, $3)     }
  | Expr GE    Expr                     { Prim(">=", $1, $3)     }
  | Expr LE    Expr                     { Prim("<=", $1, $3)     }
  | Expr OR    Expr                     { If($1, CstB(true), $3) }
  | Expr AND   Expr                     { If($1, $3, CstB(false))}
```

## Assignment 5

### Exercise 5.1

The code for merge in f sharp looks as the following (and can be found in `./assignment-5/test.fs`):

```fsharp
let merge (lst1, lst2) =
    let rec aux l1 l2 acc =
        match (l1, l2) with
        | e1 :: tail1, e2 :: _ when e1 < e2 -> aux tail1 l2 (e1 :: acc)
        | e1 :: _, e2 :: tail2 when e1 >= e2 -> aux l1 tail2 (e2 :: acc)
        | l, [] | [], l -> (acc |> List.rev) @ l 
        | _ -> acc |> List.rev
    aux lst1 lst2 []
```

The input and output looks as following:

![screenshot](./screenshots/a5_e5_1_1.png)

The code for merge in java looks as the following (and can be found in `./assignment-5/test.java`):

```java
public static int[] merge(int[] lst1, int[] lst2){
    int[] merged = new int [(lst1.length + lst2.length)];
    int cntm = 0, cnt1 = 0, cnt2 = 0;

    while (cnt1 < lst1.length && cnt2 < lst2.length){
        if (lst1[cnt1] < lst2[cnt2]) {
            merged[cntm++] = lst1[cnt1++];
        } else {
            merged[cntm++] = lst2[cnt2++];
        }
    }

    while (cnt1 < lst1.length) {
        merged[cntm++] = lst1[cnt1++];
    }

    while (cnt2 < lst2.length) {
        merged[cntm++] = lst2[cnt2++];
    }

    return merged;
}
```

The input and output looks as following:

![screenshot](./screenshots/a5_e5_1_2.png)

### Exercise 5.7

We made additions to `type` in `TypedFun.fs` in the case that matches on `ListExpr`:

```fsharp
let rec typ (e : tyexpr) (env : typ env) : typ =
    match e with
    | CstI i -> TypI
    | CstB b -> TypB
    | Var x  -> lookup env x 
    | Prim(ope, e1, e2) -> 
      let t1 = typ e1 env
      let t2 = typ e2 env
      match (ope, t1, t2) with
      | ("*", TypI, TypI) -> TypI
      | ("+", TypI, TypI) -> TypI
      | ("-", TypI, TypI) -> TypI
      | ("=", TypI, TypI) -> TypB
      | ("<", TypI, TypI) -> TypB
      | ("&", TypB, TypB) -> TypB
      | _   -> failwith "unknown op, or type error"
    | Let(x, eRhs, letBody) -> 
      let xTyp = typ eRhs env
      let letBodyEnv = (x, xTyp) :: env 
      typ letBody letBodyEnv
    | If(e1, e2, e3) -> 
      match typ e1 env with
      | TypB -> let t2 = typ e2 env
                let t3 = typ e3 env
                if t2 = t3 then t2
                else failwith "If: branch types differ"
      | _    -> failwith "If: condition not boolean"
    | Letfun(f, x, xTyp, fBody, rTyp, letBody) -> 
      let fTyp = TypF(xTyp, rTyp) 
      let fBodyEnv = (x, xTyp) :: (f, fTyp) :: env
      let letBodyEnv = (f, fTyp) :: env
      if typ fBody fBodyEnv = rTyp
      then typ letBody letBodyEnv
      else failwith ("Letfun: return type in " + f)
    | Call(Var f, eArg) -> 
      match lookup env f with
      | TypF(xTyp, rTyp) ->
        if typ eArg env = xTyp then rTyp
        else failwith "Call: wrong argument type"
      | _ -> failwith "Call: unknown function"
    | Call(_, eArg) -> failwith "Call: illegal function in call"
    | ListExpr(elist, lTyp) ->
      let ebool = List.forall(fun e -> (typ e env) = lTyp) elist
      if ebool then TypL lTyp
      else failwith "not all elements are typ"
```

### Exercise 6.1

![screenshot](./screenshots/a5_e6_1.png)

The result of the third is as expected since the function `addtwo` has its x value bound to 2.
Then `addtwo` is called with 5 as argument, which is then evaluates to 7.

The last output is a closure since the function is missing an argument `y` and therefore is not fully evaluated.

### Exercise 6.2

Added following to `eval` in `HigherFun.fs`

```fsharp
| Fun(str, e) -> 
      Clos(str, e, env)
    | Call(eFun, eArg) -> 
      let fClosure = eval eFun env
      match fClosure with
      | Closure (f, x, fBody, fDeclEnv) ->
        let xVal = eval eArg env
        let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
        in eval fBody fBodyEnv
      | Clos(str, e, env) ->
        let xVal = eval eArg env
        let bodyEnv = (str, xVal) :: env
        eval e bodyEnv
      | _ -> failwith "eval Call: not a function";;

```

Added `Clos` to type value in `HigherFun.fs`

```fsharp
type value = 
  | Int of int
  | Closure of string * string * expr * value env       (* (f, x, fBody, fDeclEnv) *)
  | Clos of string * expr * value env 
```

When it was ran it evaluated to the following:

![screenshot](./screenshots/a5_e6_2.png)

### Exercise 6.3

Added keyword `fun` in `FunLex.fsl`

```fsharp
let keyword s =
    match s with
    | "else"  -> ELSE 
    | "end"   -> END
    | "false" -> CSTBOOL false
    | "if"    -> IF
    | "in"    -> IN
    | "fun"   -> FUN
    | "let"   -> LET
    | "not"   -> NOT
    | "then"  -> THEN
    | "true"  -> CSTBOOL true
    | _       -> NAME s
```

and `Lambda` rule token in `FunLex.fsl`

```fsharp
rule Token = parse
  | [' ' '\t' '\r'] { Token lexbuf }
  | '\n'            { lexbuf.EndPos <- lexbuf.EndPos.NextLine; Token lexbuf }
  | ['0'-'9']+      { CSTINT (System.Int32.Parse (lexemeAsString lexbuf)) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*
                    { keyword (lexemeAsString lexbuf) }
  | "(*"            { commentStart := lexbuf.StartPos;
                      commentDepth := 1; 
                      SkipComment lexbuf; Token lexbuf }
  | '='             { EQ }
  | "<>"            { NE }
  | '>'             { GT }
  | '<'             { LT }
  | "->"            { LAMBDA }
  | ">="            { GE }
  | "<="            { LE }
  | '+'             { PLUS }                     
  | '-'             { MINUS }                     
  | '*'             { TIMES }                     
  | '/'             { DIV }                     
  | '%'             { MOD }
  | '('             { LPAR }
  | ')'             { RPAR }
  | eof             { EOF }
  | _               { failwith "Lexer error: illegal symbol" }
```

Added following `tokens` in `FunPar.fsy`

```fsharp
%token LAMBDA
%token FUN
```

and `precedence` for `LAMBDA` in `FunPar.fsy`

```fsharp
%right LAMBDA
```

and following `Expr:` `FUN NAME LAMBDA Expr` in `FunPar.fsy`

```fsharp 
Expr:
    AtExpr                              { $1                     }
  | AppExpr                             { $1                     }
  | IF Expr THEN Expr ELSE Expr         { If($2, $4, $6)         }
  | MINUS Expr                          { Prim("-", CstI 0, $2)  }
  | Expr PLUS  Expr                     { Prim("+",  $1, $3)     }
  | Expr MINUS Expr                     { Prim("-",  $1, $3)     }
  | Expr TIMES Expr                     { Prim("*",  $1, $3)     }
  | Expr DIV   Expr                     { Prim("/",  $1, $3)     } 
  | Expr MOD   Expr                     { Prim("%",  $1, $3)     }
  | Expr EQ    Expr                     { Prim("=",  $1, $3)     }
  | Expr NE    Expr                     { Prim("<>", $1, $3)     }
  | Expr GT    Expr                     { Prim(">",  $1, $3)     }
  | Expr LT    Expr                     { Prim("<",  $1, $3)     }
  | Expr GE    Expr                     { Prim(">=", $1, $3)     }
  | Expr LE    Expr                     { Prim("<=", $1, $3)     }
  | FUN NAME LAMBDA Expr                { Fun($2, $4)            }
```

![screenshot](./screenshots/a5_e6_3.png)

```fsharp
let e6_1 = fromString @"let add x = fun y -> x+y in add 2 5 end";;
run e6_1;;

let e6_2 = fromString @"let add = fun x -> fun y -> x+y in add 2 5 end";;
run e6_2;;
```

### Exercise 6.4

(i)

![screenshot](./screenshots/a5_e6_4_1.png)

(ii)

![screenshot](./screenshots/a5_e6_4_2.png)

### Exercise 6.5

#### part one

```fsharp
let e6_5_0 = inferType (fromString "let f x = 1 in f f end");;
val e6_5_0 : string = "int"
```

Issue is type error circularity. G is missing type.

```fsharp
let e6_5_1 = inferType(fromString "let f g = g g in f end");;
System.Exception: type error: circularity
```

```fsharp
let e6_5_2 = inferType (fromString "let f x = let g y = y in g false end in f 42 end");;
val e6_5_2 : string = "bool"
```

issue is type error bool and int. F returns both boolean and integer.

```fsharp
let e6_5_3 = inferType(fromString "let f x = let g y = if true then y else x in g false end in f 42 end");;
System.Exception: type error: bool and int
```

```fsharp
let e6_5_4 = inferType (fromString "let f x = let g y = if true then y else x in g false end in f true end");;
val e6_5_4 : string = "bool"
```

#### part two

```fsharp
let boolToBool = inferType (fromString "let f x = if x = true then true else false in f end");;
val boolToBool : string = "(bool -> bool)"
```

```fsharp
let intToInt = inferType (fromString "let f x = if x = 1 then 2 else 3 in f end");;
val intToInt : string = "(int -> int)"
```

```fsharp
let intToIntToInt = inferType (fromString "let f x = let g y = x + y in g end in f end");;
val intToIntToInt : string = "(int -> (int -> int))"
```

```fsharp
let aba = inferType (fromString "let f x = let z y = x in z end in f end");;
val aba : string = "('h -> ('g -> 'h))"
```

```fsharp
let abb = inferType (fromString "let f x = let z x = x in z end in f end");;
val abb : string = "('g -> ('h -> 'h))"
```

```fsharp
let abbcac = inferType (fromString "let f x = let y z = let v w = z (x w) in v end in y end in f end");;
val abbcac : string = "(('l -> 'k) -> (('k -> 'm) -> ('l -> 'm)))"
```

```fsharp
let ab = inferType (fromString "let f x = let y = f x in y end in f end");;
val ab : string = "('e -> 'f)"
```

```fsharp
let a = inferType(fromString "let f x = let y = f x in y end in f 1 end");;
val a : string = "'f"
```

![screenshot](./screenshots/a5_e6_5.png)

## Assignment 6

### Exercise 7.1

![screenshot](./screenshots/a6_e7_1_1.png)

#### Indication of `ex1.c` abstract syntax parts

The tree can be seen here:

```c
val it : Absyn.program =
  Prog
    [Fundec
       (None, "main", [(TypI, "n")],
        Block
          [Stmt
             (While
                (Prim2 (">", Access (AccVar "n"), CstI 0),
                 Block
                   [Stmt (Expr (Prim1 ("printi", Access (AccVar "n"))));
                    Stmt
                      (Expr
                         (Assign
                            (AccVar "n",
                             Prim2 ("-", Access (AccVar "n"), CstI 1))))]));
           Stmt (Expr (Prim1 ("printc", CstI 10)))])]
```

Below the parts which exists in the tree have been indicated:

| | Present Parts |
| --- | --- |
| Declarations | `Fundec` |
| Statements | `While`, `Block`, `Expr` |
| Types | `TypI` |
| Expressions | `Access`, `Assign`, `CstI`, `Prim1`, `Prim2` |

![screenshot](./screenshots/a6_e7_1_2.png)

### Exercise 7.2

#### Part 1

The following code is a solution for `arrsum.c`:

```c
void main(int n) {
    int arr[4];
    arr[0] = 7;
    arr[1] = 3;
    arr[2] = 9;
    arr[3] = 8;

    int sum;
    sum = 0;

    arrsum(n, arr, &sum);

    print sum;
    println;
}

void arrsum (int n, int arr[], int *sump) {
    int i;
    i = 0;

    while (i < n) {
        *sump = *sump + arr[i];
        i = i + 1;
    }
}
```

#### Part 2



### Exercise 7.3

### Exercise 7.4

### Exercise 7.5
