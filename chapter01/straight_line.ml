open Core

(* type token =
  | ID of string
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQUAL
  | NUMBER of int
  | PRINT *)

type id = string
type binop =
  | Plus
  | Minus
  | Times
  | Divide

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list
and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let prog =
  CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
              CompoundStm(AssignStm("b",
                                    EseqExp(PrintStm[IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                                            OpExp(NumExp 10, Times, IdExp "a"))),
                          PrintStm[IdExp "b"]))

(* 1 *)
let rec maxargs_stm s counter =
  match s with
  | CompoundStm (stm1, stm2) ->
    let max_stm1 = maxargs_stm stm1 counter in
      maxargs_stm stm2 max_stm1
  | AssignStm (_, e) ->
    maxargs_exp e counter
  | PrintStm el ->
    let l = List.length el in
    List.fold_left el ~init:l ~f:(fun acc e ->
        let cur_max = maxargs_exp e acc in
        if cur_max > acc
        then cur_max
        else acc)
and maxargs_exp e counter =
  match e with
  | EseqExp (st, e) ->
    let max_st = maxargs_stm st counter in
    maxargs_exp e max_st
  | IdExp _ | NumExp _ | OpExp (_, _, _) -> 0

let maxargs stmt =
  maxargs_stm stmt 0

(* 2 *)
(* let interpStm s =  *)
module String_Int_Table : sig
  type t
  val emptyTable: t
  val update: t -> id -> int -> t
  val lookup: t -> id -> int
end = struct
  type elem = string * int
  type t = elem list
  let emptyTable = []
  let update (table: t) (key: id) (value: int) =
    (key, value) :: table
  let rec lookup (table: t) (key: id) =
    match table with
    | [] -> 0
    | (k, v) :: t ->
      if k = key
      then v
      else lookup t key
end


let rec interpStm s table =
  match s with
  | CompoundStm (stm1, stm2) ->
    let t = interpStm stm1 table in
    interpStm stm2 t
  | AssignStm (i, e) ->
    let v, t = interpExp e table in
    String_Int_Table.update t i v
  | PrintStm el ->
    List.fold_left el ~init:table ~f:(fun t e ->
        let v, t = interpExp e t in
        t)
and interpExp e table =
  match e with
  | IdExp i -> (String_Int_Table.lookup table i, table)
  | NumExp n -> (n, table)
  | OpExp (e1, op, e2) ->
    let v1, t1 = interpExp e1 table in
    let v2, t2 = interpExp e2 table in
    let v =
      match op with
      | Plus -> v1 + v2
      | Minus -> v1 - v2
      | Times -> v1 * v2
      | Divide -> v1 / v2
    in
    (v, table)
  | EseqExp (st, e) ->
    let t = interpStm st table in
    interpExp e t

let interp stmt =
  interpStm stmt String_Int_Table.emptyTable
