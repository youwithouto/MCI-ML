open Core

type id = string
type binop =
  | Plus
  | Minus
  | Times
  | Div
type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list
and exp =     (* mutual recursive type definition, use `and` to connect. *)
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

(* separator of list element is `;` in OCaml, instead of `,` *)
let prog =
  CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
              CompoundStm(AssignStm("b",
                                    EseqExp(PrintStm [IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                                            OpExp(NumExp 10, Times, IdExp "a"))),
                          PrintStm[IdExp "b"]))

(* 1. maxargs : stm -> int *)
let listlength lst =
  let rec listlength_helper acc = function
    | [] -> acc
    | _ :: t -> listlength_helper (acc + 1) t
  in
  listlength_helper 0 lst;;

let rec maxargs_stm = function
  | CompoundStm (st1, st2) ->
    let max_st1 = maxargs_stm st1 in
    let max_st2 = maxargs_stm st2 in
    if max_st1 > max_st2
    then max_st1
    else max_st2
  | AssignStm (_, ex) ->
    maxargs_exp ex
  | PrintStm l ->
    let len = listlength l in
    let rec maxargs_list m = function
      | [] -> 0
      | last :: [] ->
        let cm = maxargs_exp last in
        if m > cm
        then m
        else cm
      | h :: t ->
        let cm = maxargs_exp h in
        if m > cm
        then maxargs_list m t
        else maxargs_list cm t
    in
    maxargs_list len l
and maxargs_exp = function
  | IdExp _ | NumExp _ | OpExp (_, _, _) -> 0   (* `OpExp (_, _, _)` though all ars are exps, not all types of exps can be applied *)
  | EseqExp (st, e) ->
    let max_st = maxargs_stm st in
    let max_ex = maxargs_exp e in
    if max_st > max_ex
    then max_st
    else max_ex;;


(* 2. interp : stm -> unit *)
module String_Int_Table : sig
  type t
  val emptyTable : t
  val update : t -> id -> int -> t
  val lookup : t -> id -> int
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

let rec interpStm (st:stm) (table:String_Int_Table.t) :String_Int_Table.t =
  match st with
  | CompoundStm (stm1, stm2) ->
    let newTable = interpStm stm1 table in
    interpStm stm2 newTable
  | AssignStm (i, e) ->
    let v, t = interpExp e table in
    String_Int_Table.update table i v
  | PrintStm l ->
      let rec printList = function
        | [] -> printf "\n"
        | e :: es ->
          let v, t = interpExp e table in
          printf "%d " v;
          printList es in
      printList l;
      table
and interpExp (ex:exp) (table:String_Int_Table.t) : int * String_Int_Table.t =
  match ex with
  | IdExp i -> (String_Int_Table.lookup table i, table)
  | NumExp n -> (n, table)
  | OpExp (e1, o, e2) ->
    begin
    let v1, t1 = interpExp e1 table in
    let v2, t2 = interpExp e2 table in
    let v =
      match o with
      | Plus -> v1 + v2
      | Minus -> v1 - v2
      | Times -> v1 * v2
      | Div -> v1 / v2
    in
    (v, table)
    end
  | EseqExp (st, ex) ->
    let newTable = interpStm st table in
    interpExp ex newTable

let interp (st:stm) : unit =
  let table = interpStm st String_Int_Table.emptyTable in 
  ()
