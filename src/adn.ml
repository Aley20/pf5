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

let rec count_occ x l =
  match l with 
  | [] -> 0
  | hd :: tl -> if hd==x then 1+count_occ x tl else count_occ x tl


  let max_occ l =
    let max_occs = ref 0 in
    let rec check_all_occ list = 
      match list with
      | [] -> true
      | hd :: tl ->
        let x = count_occ hd l in
        if  x > !max_occs then max_occs := x;
        x = !max_occs && check_all_occ tl
    in
    if check_all_occ l then 0
    else !max_occs
  
  
let rec consensus (list : 'a list) : 'a consensus =
  match list with
  | [] -> No_consensus
  | hd :: tl -> 
    let x= count_occ hd list in
    let y=max_occ list in
    if x=List.length list then Full hd 
    else if y=0 then No_consensus else if y>0 then Partial (hd,y) else consensus tl


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

let rec consensus_sequence (ll : 'a list list) : 'a consensus list = 
  match ll with
  | [] -> []
  | hd :: tl -> [consensus hd]@consensus_sequence tl

let p=consensus_sequence [[1; 1; 1; 1];
[1; 1; 1; 2];
[1; 1; 2; 2];
[1; 2; 2; 2]]

let m=consensus_sequence[['a';'c';'g';'t']]
(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)