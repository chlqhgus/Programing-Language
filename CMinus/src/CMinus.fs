module CMinus

open AST
open Types

// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false
  | Var s -> s 

  | Add (x,y) -> 
    match evalExp x mem, evalExp y mem with
    | Int i1, Int i2 -> Int (i1+i2)
    | _ -> Int (-1)

  | Sub (x,y) ->
    match evalExp x mem, evalExp y mem with
    | Int i1, Int i2 -> Int (i1-i2)
    | _ -> Int (-1)

  | LessThan (x,y) -> 
    match evalExp x mem, evalExp y mem with
    | Int i1, Int i2 -> Bool (i1 < i2)
    | _ -> Bool false
  
  | GreaterThan (x,y) ->
    match evalExp x mem, evalExp y mem with
    | Int i1, Int i2 -> Bool (i1 > i2)
    | _ -> Int (-1)

  | Equal (x,y) -> Bool (evalExp x mem = evalExp y mem)
  | NotEq (x,y) -> Bool (evalExp x mem <> evalExp y mem)
  | _ -> Int (-1) // TODO: fill in the remaining cases. 

// Note: You may define more functions.

// Execute a statement and return the updated memory.
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  | Assign (x,e)  -> Map.add  x (evalExp e mem) mem
  | Seq (s1,s2) ->
      let m1 = exec s1 mem
      let m2 = exec s2 m1
      m2
  | If (e,s1,s2) ->
      let tf = evalExp e mem
      let mem_t = exec s1 mem
      let mem_f = exec s2 mem
      match tf with
      | Bool b -> if (b) then mem_t else mem_f
  | While (e,s) -> 
      let tf = evalExp e mem
      match tf with
      | Bool b -> 
        if (b) then
          let m1 = exec s mem
          let m2 = exec (While (e,s)) (exec s mem) 
          m2
        else
          mem
  | _ -> raise UndefinedSemantics // TODO: fill in the remaining cases.

// The program starts execution with an empty memory. Do NOT fix this function.
let run (prog: Program) : Mem =
  exec prog Map.empty
