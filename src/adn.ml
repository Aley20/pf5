type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  List.init (String.length str) (String.get str)


(* conversions *)
let base_of_char (c : char) : base =
  match c with
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | _   -> WC 


let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s)


let string_of_dna (seq : dna) : string =
  match seq with
  | [] -> ""
  | _  -> String.concat "" (List.map string_of_base seq)



(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)

(* 1er cas : Si slice est vide alors list est un suffixe
   2eme cas : Si 1er élément de slice(x1) est égal au 1er élément de list(x2) alors on va réappeler récursivement
              cut_prefix avec le reste de slice(y1) et list(y2) jusqu'a ce que l'une des deux listes soit entiérement parcourus
   3eme cas : Sinon (si la liste pre n’est pas un préfixe de l) on renvoie None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match (slice,list) with 
  | ([],suf) -> Some suf 
  | (x1 :: y1, x2 :: y2) when x1=x2 -> cut_prefix y1 y2 
  | (_,_) -> None

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  failwith "À compléter"
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
  (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  failwith "À compléter"
  

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) =
  failwith "A faire"

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)

   let rec count_occ x l= 
    match l with
    | [] -> 0
    | hd :: tl -> (if hd = x then 1 else 0) + count_occ x tl
  
   let max_occurrence_info lst =
  let rec aux max_count occurrences_list = function
  | [] -> occurrences_list
  | hd :: tl ->
      let occurrences = count_occ hd lst in
      if occurrences > max_count then
        aux occurrences [(hd, occurrences)] tl
      else if occurrences = max_count && not (List.exists (fun (x, _) -> x = hd) occurrences_list) then
        aux max_count ((hd, occurrences) :: occurrences_list) tl
      else
        aux max_count occurrences_list tl
in
match lst with
| [] -> failwith "Empty list"
| _ ->
  let max_occurrences_list = aux 0 [] lst in
  match max_occurrences_list with
  | [] -> [(List.hd lst, 0)]
  | _ -> List.rev max_occurrences_list;;


(*
   type base = A | C | G | T | WC (* wildcard *)

let a= max_occurrence_info ['a';'b';'c';'c';'b';'a']
let b=max_occurrence_info [A; A; G; G; T] 
*)

(* Accéder à 'a' dans la paire ('a', 2) *)

let sort_and_check_max lst =
  let sorted_list = List.sort (fun (_, count1) (_, count2) -> compare count1 count2) lst in
  match sorted_list with
  | [] -> [(Obj.magic 0,0)]
  | [(x, count)] -> [(x, count)]
  | (x1, count1) :: (x2, count2) :: tl ->
      if count1 = count2 then [(x1, 0)]
      else [(x1, count1)]
;;

(* Utilisation *)
(*
let result = sort_and_check_max a;;
let res = sort_and_check_max b
*)

let rec consensus (list : 'a list) : 'a consensus =
  match list with
  | [] -> No_consensus
  | [x] -> Full x
  | hd :: tl -> let max=max_occurrence_info list in
      match sort_and_check_max max with 
      | [] -> No_consensus
      | [(value,1)] -> Full value
      | [(value,count)] -> if count = List.length list then Full value else if count=0 then No_consensus else Partial (value,count)
      | _ -> No_consensus


(*
let r=consensus ['a';'b';'c';'c';'b';'a']
let rr=consensus ['a';'a';'a';'a';'a';'a']
let rrr=consensus [C; C; T; C] 
let  rrrr=consensus [A; A; G; G; T] 
let  rrrrr=consensus [] 
*)


(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

    
    let transpose_lists lst =
      let rec transpose_aux acc = function
        | [] -> List.rev acc
        | [] :: _ -> List.rev acc
        | lists -> transpose_aux (List.map List.hd lists :: acc) (List.map List.tl lists)
      in
      transpose_aux [] lst
    
    (* Utilisation *)
    (*let input_list = [["x0"; "x1"; "x2"]; ["y0"; "y1"; "y2"]; ["z0"; "z1"; "z2"]];;
    let i=[["a";"c";"g";"t"]]
    let rr=transpose_lists i
    let result = transpose_lists input_list;;*)

    let rec consensus_sequence (ll : 'a list list) : 'a consensus list =
      let transposed_lists = transpose_lists ll in
      let rec process_columns acc = function
        | [] -> List.rev acc
        | col :: cols -> process_columns (consensus col :: acc) cols
      in
      process_columns [] transposed_lists
    ;;
    

  (*let c=consensus ["a"]
  let a=transpose_lists [["a";"c";"g";"t"]]
  let e=consensus_sequence [["a";"c";"g";"t"]]
  let aa=transpose_lists [["a";"a";"a";"a"];["a";"a";"a";"t"];["a";"a";".";"."]]
  let ee=consensus_sequence [["a";"a";"a";"a"];["a";"a";"a";"t"];["a";"a";".";"."]]
  *)
                     (*
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)

(* Utilisation *)
(*let input_list = [["x0x1x2"]; ["y0y1y2"]; ["z0z1z2"]];;
let result = transpose_lists input_list;;
*)

