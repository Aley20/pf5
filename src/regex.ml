open Regex_base

let rec repeat n l =
  if n<=0 then []
  else l @ repeat (n-1) l

let rec expr_repeat n e =
  if n<=0 then Eps
  else Concat (e,expr_repeat (n-1) e)

let rec is_empty e =
  failwith "À compléter"

let rec null e =
  failwith "À compléter"

let rec is_finite e =
  let rec is_finite_helper a b=
  if List.mem b a then false else
    match b with
    | Eps -> true
    | Base _ -> true
    | Joker -> true
    | Concat (x1,x2) -> is_finite_helper a x1 && is_finite_helper a x2
    | Alt (x1,x2) -> is_finite_helper a x1 && is_finite_helper a x2
    | Star x -> true
  in is_finite_helper [] e 

let product l1 l2 =
  failwith "À compléter"

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
