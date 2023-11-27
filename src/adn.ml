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
let cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  failwith "A faire"

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let split_at_position pos lst =
  let rec aux acc n = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if n = 0 then (List.rev acc, l)
        else aux (h :: acc) (n - 1) t
  in
  aux [] pos lst

let rec first_occ slice l =
  let rec loop i =
    if List.length slice + i <= List.length l then
      let before, after = split_at_position i l in
      let removed, after = split_at_position (List.length slice) after in
      if removed = slice then
        (before, removed, after)
      else
        loop (i + 1)
    else
      ([],[],[])
  in
  loop 0
  (*let () =
  let slice = ['A'; 'G'] in
  let l = ['A'; 'A'; 'A'; 'G'; 'T'; 'C'; 'A'; 'A'; 'A'; 'G'; 'T'; 'C'] in

  Printf.printf "Original list: [%s]\n" (String.concat "; " (List.map Char.escaped l));

  let before, removed, after = first_occ slice l in

  Printf.printf "([%s][%s])\n" (String.concat "; " (List.map Char.escaped before)) (String.concat "; " (List.map Char.escaped after))
    
    *)
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

let extraire_liste start stop l acc =
  try
    let (_, _, remaining_after_start) = first_occ start l in
    let (between, _, _) = first_occ stop remaining_after_start in
    acc := !acc @ [between];  (* ajouter la liste de caractères à l'accumulation *)
    remaining_after_start
  with Not_found -> []
  let rec slices_between_ter start stop l acc =
  if List.length l < (List.length start + List.length stop) then
    !acc  
  else
    let remaining = extraire_liste start stop l acc in
    slices_between_ter start stop remaining acc
let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
   if List.length l<List.length start+List.length stop then failwith"La liste est courte" else
    match l with
    |[]->failwith "La liste est vide"
    |_->slices_between_ter start stop l (ref [])
(* Test 
let () =
  let start = ['A'; 'G'] in
  let stop = ['T'; 'C'] in
  let l = ['A'; 'A'; 'A'; 'G'; 'A'; 'T'; 'C'; 'A'; 'A'; 'A';'C'; 'A'; 'G';'C'; 'A'; 'T'; 'C'] in

  Printf.printf "Original list: [%s]\n" (String.concat "; " (List.map Char.escaped l));

  let result = slices_between start stop l in
  Printf.printf "Slices between start and stop: [%s]\n"
    (String.concat "; " (List.map (fun sublist -> "[" ^ (String.concat "; " (List.map Char.escaped sublist)) ^ "]") result))*)
(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) =
  
let cut_genes (strand : dna) : string list =
  if List.length strand = 0 then
    failwith "None"
  else
    let start = [A; T; G] in
    let stop = [T; A; A] in 
    let gene_lists = slices_between start stop strand in
    List.map string_of_dna gene_lists
(*test
let () = 
  let strand = dna_of_string "ATGCCTGGGCATTGAGATCATTGGCACCCTGCATAAGATGTGTGACTGTAGAGCTCTTCCTGAC..CATGCATAAAGAATGCCAATGGCACAGCCTGGTATCTTTGCCATAAATGGCTCCTGGTGGAGCTGATAGTCACTTTCCATAATTAATGCATGGTGGTGGAGTTATTCTTGACTTTCCATAA" in 
  (* Appel à la fonction cut_genes *)
  let genes_as_dna = cut_genes strand in 
  (* affichage des gènes sous forme de séquences d'ADN *)
  Printf.printf "Genes as DNA sequences: [%s]\n"
    (String.concat "; \n" genes_as_dna);
  *)

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
let consensus (list : 'a list) : 'a consensus =
  failwith "À compléter"

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

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  failwith "À compléter"

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
