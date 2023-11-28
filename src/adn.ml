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
let split_at_position pos lst =
 let rec aux acc n = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if n = 0 then (List.rev acc, l)
        else aux (h :: acc) (n - 1) t
 in
 aux [] pos lst
  
let first_occ slice l =
 let rec loop i =
    if List.length slice + i <= List.length l then
      let before, after = split_at_position i l in
      let removed, after = split_at_position (List.length slice) after in
      if removed = slice then
        Some (before, removed, after)
      else
        loop (i + 1)
    else
      None
 in
 loop 0
(*
 first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
 first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
 first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

let rec extraire_liste start stop l acc =
 match first_occ start l with
 | None -> []
 | Some (before, _, after) ->
    match first_occ stop after with
    | None -> []
    | Some (_, between, after) ->
      acc := !acc @ [between];
      after

let rec slices_between start stop l =
 match l with
 | [] -> []
 | _ ->
    let result = ref [] in
    let remaining = extraire_liste start stop l result in
    if remaining = [] then !result else !result @ slices_between start stop remaining
    
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
   Appel à la fonction cut_genes 
  let genes_as_dna = cut_genes strand in 
   affichage des gènes sous forme de séquences d'ADN 
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

   (* Compte occurence de l'élémént x *)
   let rec count_occ x l= 
    match l with
    | [] -> 0
    | hd :: tl -> (if hd = x then 1 else 0) + count_occ x tl
  
  (* va renvoyer sous forme de liste de tuple (a * int) tous les éléments de la liste avec leurs occurrences (element,occurence)*)
  let list_occurrence lst =
    let rec aux acc = function (* ajoute les tuples (élément, nombre d'occurrences) dans la liste acc.*)
      | [] -> acc
      | hd :: tl ->
        let count = count_occ hd lst in
        aux ((hd, count) :: acc) tl
    in
    match lst with
    | [] -> failwith "Empty list"
    | _ ->
      let result = aux [] lst in
      List.fold_left (fun acc (x, _) -> (* List.fold_left nécessaire nécessaire pour construire la liste final*)
        (* on verifie si x est deja dans le tuple , si c'est le cas on fait rien sinon on ajoute un nouveau tuple dans notre liste acc *)
        if not (List.exists (fun (y, _) -> x = y) acc) then (x, count_occ x lst) :: acc
        else acc
        )[] result


(*
   type base = A | C | G | T | WC (* wildcard *)

let a= max_occurrence_info ['a';'b';'c';'c';'b';'a']
let b=max_occurrence_info [A; A; G; G; T] 
*)

(* va trié notre liste de tuples  *)

let trie_list_tuples lst =
  let sorted_list = List.sort (fun (_, count2) (_, count1) -> compare count1 count2) lst in (* trie notre liste de tuples lst*)
  match sorted_list with
  | [] -> [(Obj.magic 0,0)]
  | [(x, count)] -> [(x, count)] 
  | (x1, count1) :: (x2, count2) :: tl ->
      if count1 = count2 then [(x1, 0)] 
      else [(x1, count1)]
;;

let rec consensus (list : 'a list) : 'a consensus =
  match list with
  | [] -> No_consensus
  | [x] -> Full x
  | hd :: tl -> let l_occ=list_occurrence list in
      match trie_list_tuples l_occ with 
      | [] -> No_consensus
      | [(value,count)] -> if count = List.length list then Full value else if count=0 then No_consensus else Partial (value,count)
      | _ -> No_consensus

(*let u=max_occurrence_info ['a';'b';'a';'c';'c';'a']
let s=sort_and_check_max u
let p=consensus ['a';'b';'a';'c';'c';'a']
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

    (* pour une liste [["x0"; "x1"; "x2"]; ["y0"; "y1"; "y2"]; ["z0"; "z1"; "z2"]] la fonction transpose_lists va donner 
       [["x0"; "y0"; "z0"]; ["x1"; "y1"; "z1"]; ["x2"; "y2"; "z2"]]*)
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
        | hd :: tl -> process_columns (consensus hd :: acc) tl
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

