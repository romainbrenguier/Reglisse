(* ocamlbuild -tag use_ocaml-aiger -tag use_ocaml-cudd examples/mult_bool_matrix.byte -- *)

open Speculog
open Expression


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

  
let vector_mult_aiger_table = Hashtbl.create 10

let vector_mult_aiger size = 
  try Hashtbl.find vector_mult_aiger_table size 
  with Not_found ->
    let u = var "u" (Type.array Type.bool size) in
    let v = var "v" (Type.array Type.bool size) in
    let res = var "res" Type.bool in
    let sum = 
      let rec aux accu k = 
	if k = size then accu
	else 
	  let prod = conj (get u (int k)) (get v (int k)) in
	  aux (disj accu prod) (k+1)
      in aux (conj (get u (int 0)) (get v (int 0))) 1
    in
    let aiger = functional_synthesis [res,sum] in
    Hashtbl.add vector_mult_aiger_table size aiger;
    aiger

let simple_vector_mult u v w output =
  let aig = vector_mult_aiger (Expression.size u) in
  let name = Common.tmp_name () in
  let aig = use_module aig ~inputs:["u",u;"v",v] ~outputs:["res",name]
    (function [res] -> functional_synthesis [output, neg (equiv w res)])
  in
  Aiger.full_hide aig name
    
  
let generate sequential size_m size_n size_o =
  let mA = var "a" (Type.array (Type.array Type.bool size_n) size_m) in
  let mB = var "b" (Type.array (Type.array Type.bool size_o) size_n) in
  let mC = var "controllable_c" (Type.array (Type.array Type.bool size_o) size_m) in
  let mD = var "d" (Type.array (Type.array Type.bool size_o) size_m) in
  let init = var "init" Type.bool in

  let a i j = get (get mA (int i)) (int j) in
  let b i j = get (get mB (int i)) (int j) in
  let c i j = get (get mC (int i)) (int j) in
  let d i j = get (get mD (int i)) (int j) in
  let e = var "errors" (Type.array Type.bool (size_m*size_o)) in

  let error n = 
    let i,j = n mod size_m, n / size_m in
    (*let sum = 
      let rec aux accu k = 
	if k = size_n then accu
	else 
	  let prod = conj (a i k) (b k j) in
	  aux (disj accu prod) (k+1)
      in aux (conj (a i 0) (b 0 j)) 1
    in
    functional_synthesis *)

    (* The matrix B should be transposed *)
    simple_vector_mult (get mA (int i)) (get mB (int j)) (c i j) (get e (int n))
  in 
  
  let update n = 
    let i,j = n mod size_m, n / size_m in
    [b i j, ite init (c i j) (if i = j then bool true else bool false); d i j, b i j]
  in
      
  let updates = 
    let rec loop n accu = 
      if n = size_m * size_o
      then functional_synthesis accu
      else let aig = List.rev_append (update n) accu in loop (n+1) aig
    in
    if sequential then loop 0 [] else Aiger.empty
  in


  let global_error = 
    let rec loop n accu = 
      if n = size_m * size_o
      then accu
      else let aig = Aiger.compose accu (error n) in loop (n+1) aig
    in loop 0 updates
  in

  let e = var "errors" (Type.array Type.bool (size_m * size_o)) in

  let with_errors = Aiger.compose global_error (for_some e) in
  let rec loop_hide accu n = 
    if n = size_m * size_o then accu
    else
      (
	let i,j = n mod size_m, n / size_m in
	let aig = Aiger.full_hide accu ("errors["^string_of_int n^"]") in
	let aig = if sequential then Aiger.full_hide aig ("d["^string_of_int i^"]["^string_of_int j^"]") else aig in
	loop_hide aig (n+1)
      )
  in
  loop_hide with_errors 0




let initial i j = (i + j) mod 2 = 0

let vector_mult u v init initial output =
  let aig = vector_mult_aiger (Expression.size u) in
  let name = Common.tmp_name () in
  let aig = use_module aig ~inputs:["u",u;"v",v] ~outputs:["res",name]
    (function [res] -> functional_synthesis [output, 
					     disj res 
					       (conj (neg init) initial)
					    (*res*)
					     (*ite init res initial*)
					    ])
  in
  Aiger.full_hide aig name

    

let generate_dyn size_m size_n =
  make_latch "a" (Type.array (Type.array Type.bool size_n) size_m)
    (fun mA mA_next ->
      make_latch "init" Type.bool 
	(fun init init_next ->
  (* columns and lines of b and c are inverted 
     (ie we consider the transpose of the matrix *)
  let mB = var "b" (Type.array (Type.array Type.bool size_n) (size_n/2)) in
  let mC = var "controllable_c" (Type.array (Type.array Type.bool size_n) (size_n - size_n / 2)) in


  let a i j = get (get mA (int i)) (int j) in
  let next_a i j = get (get mA_next (int i)) (int j) in
  (* the controller controls only some columns *)
  let b i j = 
    if j >= size_n / 2
    then get (get mC (int (j - size_n / 2))) (int i)
    else get (get mB (int j)) (int i)
  in
  let column_b j =
    if j >= size_n / 2
    then get mC (int (j - size_n / 2)) 
    else get mB (int j)
  in

  let mult i j = 
    vector_mult (get mA (int i)) (column_b j) init (bool (initial i j)) (next_a i j) 
  in

  (* the error condition is that one line is totaly 1 or totaly 0 *)
  let error n =
    let i = n / 2 in 
    let value = i mod 2 = 0 in
    let rec aux accu k =
      if k = size_n then accu
      else aux (conj accu (equals (a i k) (bool value))) (k+1)
    in 
    aux init 0
  in
  
  let error_vector = array (Array.init (2*size_m) error) in

  let rec loop accu i j = 
    if i = size_m then accu 
    else if j = size_n then loop accu (i+1) 0
    else 
      let m = mult i j in
      loop (Aiger.compose accu m) i (j+1)
  in 
  let aig = loop (functional_synthesis [init_next,bool true]) 0 0 in
  Aiger.compose (for_some error_vector) aig
	 )
	)


let generate_add size_m size_n =
  let mA = var "a" (Type.array (Type.array Type.bool size_n) size_m) in
  let mB = var "b" (Type.array (Type.array Type.bool size_n) size_m) in
      let mC = var "controllable_c" (Type.array (Type.array Type.bool size_n) size_m) in

  let a i j = get (get mA (int i)) (int j) in
  let b i j = get (get mB (int i)) (int j) in
  let c i j = get (get mC (int i)) (int j) in
  let e = var "errors" (Type.array Type.bool (size_m*size_n)) in

  let error n = 
    let i,j = n mod size_m, n / size_m in
    functional_synthesis [get e (int n), neg (equiv (c i j) (disj (a i j) (b i j)))]
  in 

  let global_error = 
    let rec loop n accu = 
      if n = size_m * size_n
      then accu
      else let aig = Aiger.compose accu (error n) in loop (n+1) aig
    in loop 0 Aiger.empty
  in
  let e = var "errors" (Type.array Type.bool (size_m * size_n)) in

  let with_errors = Aiger.compose global_error (for_some e) in
  let rec loop_hide accu n = 
    if n = size_m * size_n then accu
    else
      (
	let aig = Aiger.full_hide accu ("errors["^string_of_int n^"]") in
	loop_hide aig (n+1)
      )
  in
  loop_hide with_errors 0
    

let main =
  (*Common.display_debug := true;*)
  let max = 2 in
  Common.iter 2 max (fun i () -> 
    let file_name = "matrix/for_some_"^string_of_int i^".aag" in
    print_endline ("writing aiger to "^file_name);
    let e = var "errors" (Type.array Type.bool i) in
    Aiger.write_to_file (for_some e) file_name
  ) ();

  Common.iter 2 max (fun i () -> 
    Common.iter 2 max (fun j () -> 
      let file_name = "matrix/mult_bool_matrix_dyn_"^string_of_int i^"_"^string_of_int j^".aag" in
      print_endline ("writing aiger to "^file_name);
      Aiger.write_to_file (generate_dyn i j) file_name
    ) ()
  ) ();

  Common.iter 2 max (fun i () -> 
    Common.iter 2 max (fun j () -> 
      let file_name = "matrix/mult_bool_matrix_seq_"^string_of_int i^"_"^string_of_int j^".aag" in
      print_endline ("writing aiger to "^file_name);
      Aiger.write_to_file (generate true i i j) file_name
    ) ()
  ) ();


  let aux i j k = 
    let file_name = "matrix/mult_bool_matrix_"^string_of_int i^"_"^string_of_int j^"_"^string_of_int k^".aag" in
    print_endline ("writing aiger to "^file_name);
    Aiger.write_to_file (generate false i j k) file_name;
  in

  for i = 30 to 100
  (*do for j = 10 to i - 1
     do for k = 9 to j - 1
	do 
	  aux i j k;
	  aux i k j;
	  aux j i k;
	  aux j k i;
	  aux k i j;
	  aux k j i;
	done;
	aux i j j;
	aux i i j;
	aux j i i;
	aux j j i;
     done;*)
  do
     aux i i i
  done;
	  


  Common.iter 2 max (fun i () -> 
    Common.iter 2 max (fun j () -> 
      let file_name = "matrix/add_bool_matrix_"^string_of_int i^"_"^string_of_int j^".aag" in
      print_endline ("writing aiger to "^file_name);
      Aiger.write_to_file (generate_add i j) file_name
    ) ()
  ) ()
