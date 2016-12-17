(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/scheduling.byte -- *)
open Speculog
open Expression
open RegularExpression

(* reactivity of process i within d steps *)
let reactivity i d = 
  alt
    (List.map 
       RegularExpression.of_string 
       [
	 (*
	 (* fill_i should be scheduled within d steps of push_i *)
	 Printf.sprintf "{true} * {push_%d} ({! controllable_fill_%d} %d)" i i d;
	 (* empty_i should be scheduled d steps after fill_i *)
	 Printf.sprintf "{true} * {controllable_fill_%d} ({true} %d) ({! controllable_empty_%d} %d)" i d i d;
	 (* empty_i should not be scheduled to early *)
	 Printf.sprintf "{true} * {controllable_fill_%d} {true} (({true}?) %d) {controllable_empty_%d}" i (d-2) i;
	 (* the task b should not be launched if there was no push *)
	 Printf.sprintf "{true} * {! push_%d} %d {controllable_fill_%d}" i d i;
	  *)
	 Printf.sprintf "{true} * ({push_%d} ({! controllable_fill_%d} %d)) | 
			 ({controllable_fill_%d} ({true} %d) ({! controllable_empty_%d} %d)) 
			 | ({controllable_fill_%d} {true} (({true}?) %d) {controllable_empty_%d}) | ({! push_%d} %d {controllable_fill_%d}) "
			i i d i d i d i (d-2) i i d i;
    ])

let mutual_exclusion_2 n =
  (* task_i and task_j should not be scheduled at the same time *)
  let rec aux accu i = 
    if i >= n / 2 then accu
    else 
      aux
	(RegularExpression.of_string 
	   (Printf.sprintf "{true} * {controllable_fill_%d & controllable_fill_%d}" (2*i) (2*i+1))
	 :: accu
	) (i+1)
  in aux [] 0

let mutual_exclusion_3 n =
  let rec aux accu i = 
    if i >= n / 3 then accu
    else 
      aux
	(RegularExpression.of_string 
	   (Printf.sprintf "{true} * {controllable_fill_%d & controllable_fill_%d  & controllable_fill_%d}" (3*i) (3*i+1) (3*i+2))
	 :: accu
	) (i+1)
  in aux [] 0

let mutual_exclusion_4 n =
  let rec aux accu i = 
    if i >= n / 4 then accu
    else 
      aux
	(RegularExpression.of_string 
	   (Printf.sprintf "{true} * {controllable_fill_%d & controllable_fill_%d  & controllable_fill_%d  & controllable_fill_%d }" (4*i) (4*i+1) (4*i+2) (4*i+3))
	 :: accu
	) (i+1)
  in aux [] 0

(* if one of the [n] tasks is running then the light should be on *)
let light n =
  let tasks = 
    let rec loop accu i = 
      if i > n then accu else loop (("controllable_fill_"^string_of_int i)::accu) (i+1)
    in loop [] 1
  in
  RegularExpression.of_string 
    ("{true} * {! controllable_light <-> ("
      ^
	Common.list_to_string tasks " | " 
      ^" ) }"

    )


let or_module = Aiger.read_from_file "matrix/or.aag"

(* Takes the disjuction inside an array *)
let for_some e = 
  let size = Expression.size e in
  let rec loop previous i = 
    if i >= size then Aiger.empty
    else
      let imported = 
	use_module or_module 
	  ~inputs:["a", previous; "b", get e (int i)]
	  ~outputs:["c",
		    if i < size - 1 
		    then "tmp["^string_of_int i^"]" 
		    else "err"
		   ]
	  (function [c] -> loop c (i+1))  
      in 
      if i < size - 1
      then Aiger.full_hide imported ("tmp["^string_of_int i^"]")
      else imported 
  in 
  loop (get e (int 0)) 1 
    
let spec n d p =
  let common = 
    if p = 1 then [light n]
    else if p = 2 then light n :: mutual_exclusion_2 n
    else if p = 3 then light n :: mutual_exclusion_3 n
    else if p = 4 then light n :: mutual_exclusion_4 n
    else failwith "p should be smaller than 4"
  in
  let rec loop accu i = 
    if i > n then accu
    else loop (reactivity i d :: accu) (i+1)
  in
  let expr_list = loop common 1 in

  let aig,_ = 
    List.fold_left 
      (fun (aig,i) e -> 
       let a = to_aiger ~prefix:("auto"^string_of_int i) e in
       Aiger.compose aig a, i+1
      ) (Aiger.empty,1) expr_list  
  in 

  let aig, no_accept,_ =
    List.fold_left 
      (fun (aig,gate,i) s -> 
       let prefix = "auto"^string_of_int i in
       let aig, v = Aiger.new_var aig in
       let lit_state = Aiger.symbol2lit aig (prefix^"accept",Some 0) in
       let aig = Aiger.add_and aig (Aiger.var2lit v) gate (Aiger.aiger_not lit_state) in
       let aig = Aiger.hide aig (prefix^"accept",Some 0) in
       aig, (Aiger.var2lit v), i+1
				   (*aig,gate,i*)
      ) (aig,Aiger.aiger_true,1) expr_list
  in

  Aiger.add_output aig (Aiger.aiger_not no_accept) ("accept",None)

  (* alt (loop common 1)*)

let main = 
  for m = 1 to 50 do
    for d = 5 to 5 do
      for p = 1 to 4 do
	let n = p * m in
	let file_name = "cycles/cycle_sched_"^string_of_int n^"_"^string_of_int d^"_"^string_of_int p^".aag" in
	print_endline ("writing aiger to "^file_name);
	let aig = spec n d p in
	Aiger.write_to_file aig file_name
      done;
    done;
  done

