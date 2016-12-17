(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/matrix_mult.byte -- *)

open Speculog open Expression

let aiger_mult int_size = Aiger.read_from_file ("matrix/mult"^string_of_int int_size^".aag")

let prod int_size i k j = 
  Aiger.full_rename (aiger_mult int_size)
    ["a",("a["^string_of_int i^"]["^string_of_int k^"]");
     "b",("b["^string_of_int k^"]["^string_of_int j^"]");
     "c",("prod_"^string_of_int i^"_"^string_of_int k^"_"^string_of_int j)
    ] 


let adder size1 size2 =
  let a = var "a" (Type.int size1) in
  let b = var "b" (Type.int size2) in
  let c = var "c" (Type.int (1 + size2)) in
  functional_synthesis [c, add a b]

let add int_size matrix_size i j =
  let rec aux accu k = 
    if k = matrix_size then accu 
    else 
      let sum = Aiger.full_rename (adder (2 * int_size) (2 * int_size+k-1))
	[
	  "a","prod_"^string_of_int i^"_"^string_of_int k^"_"^string_of_int j;
	  "b","tmpadd_"^string_of_int i^"_"^string_of_int (k-1)^"_"^string_of_int j;
	  "c","tmpadd_"^string_of_int i^"_"^string_of_int k^"_"^string_of_int j;
	]
      in 
      let composed = Aiger.compose accu sum in 
      let hiden = Aiger.full_hide composed
	("tmpadd_"^string_of_int i^"_"^string_of_int (k-1)^"_"^string_of_int j) 
      in
      aux hiden (k+1)
  in 
  let aiger_init =
    Aiger.full_rename (adder (2 * int_size) (2 * int_size))
      [
	"a","prod_"^string_of_int i^"_0_"^string_of_int j;
	"b","prod_"^string_of_int i^"_1_"^string_of_int j;
	"c","tmpadd_"^string_of_int i^"_1_"^string_of_int j;
      ]
  in
  Aiger.full_rename (aux aiger_init 2)
    [
      "tmpadd_"^string_of_int i^"_"^string_of_int (matrix_size - 1)^"_"^string_of_int j, "sum["^string_of_int i^"]["^string_of_int j^"]"
    ]


let not_equals int_size matrix_size =
  let size = 2 * int_size + matrix_size - 1 in
  let mS = var "sum" (Type.array (Type.array (Type.int size) matrix_size) matrix_size) in
  let mC = var "controllable_c" (Type.array (Type.array (Type.int size) matrix_size) matrix_size) in
  let err = var "err" Type.bool in
  let s i j = get (get mS (int i)) (int j) in
  let c i j = get (get mC (int i)) (int j) in
  let e = 
    for_some [0,matrix_size] (fun i ->
      for_some [0,matrix_size] (fun j ->
	neg (equals (s i j) (c i j)))) 
  in
  functional_synthesis [err, e]

let global int_size matrix_size =
  let rec loop_matrix accu i j =
    if i = matrix_size then accu
    else if j = matrix_size then loop_matrix accu (i+1) 0
    else
      let rec loop accu k = 
	if k = matrix_size then accu
	else 
	  let c = Aiger.compose accu (prod int_size i k j) in
	  loop c (k+1)
      in 
      let prods = loop accu 0 in
      let a = Aiger.compose prods (add int_size matrix_size i j) in
      (*print_endline " ----- ACCU ----";
      Aiger.write accu stdout;
      print_endline " ----- PRODS ----";
      Aiger.write prods stdout;
      print_endline " ----- ADD ----";
      Aiger.write (add i j) stdout;
      print_endline " ----- A ----";
      Aiger.write a stdout;*)

      loop_matrix a i (j+1)
  in 
  
  let m = loop_matrix (not_equals int_size matrix_size) 0 0 in

  let rec loop_hide accu i j k =
    if i = matrix_size then accu
    else if j = matrix_size then loop_hide accu (i+1) 0 0 
    else if k = matrix_size 
    then 
      let h = 
	Aiger.full_hide accu ("sum["^string_of_int i^"]["^string_of_int j^"]") 
      in
      loop_hide h i (j+1) 0
    else
      let h = 
	Aiger.full_hide accu ("prod_"^string_of_int i^"_"^string_of_int k^"_"^string_of_int j) 
      in
      loop_hide h i j (k+1)
  in    
  try loop_hide m 0 0 0 
  with Not_found -> prerr_endline "Warning: Not_found raised in loop_hide"; m

		   
let main = 
  for i = 1 to 2 do
    for j = 5 to 10 do 
      let file_name = "matrix/matrix_mult_"^string_of_int i^"_"^string_of_int j^".aag" in
      print_endline ("writing aiger to "^file_name);
      Aiger.write_to_file (global i j) file_name;
    done;
  done;
  (*let file_name = "matrix/matrix_equals.aag" in
  print_endline ("writing aiger to "^file_name);
  Aiger.write_to_file not_equals file_name;
  Common.iter 0 (matrix_size - 1) (fun i () -> 
    Common.iter 0 (matrix_size - 1) (fun j () -> 
      Common.iter 0 (matrix_size - 1) (fun k () -> 
	let file_name = "matrix/matrix_prod_"^string_of_int i^"_"^string_of_int k^"_"^string_of_int j^".aag" in
	print_endline ("writing aiger to "^file_name);
	Aiger.write_to_file (prod i k j) file_name
      ) ()
    ) ()
  ) ();
  Common.iter 0 (matrix_size - 1) (fun i () -> 
    Common.iter 0 (matrix_size - 1) (fun j () ->
      let file_name = "matrix/matrix_add_"^string_of_int i^"_"^string_of_int j^".aag" in
      print_endline ("writing aiger to "^file_name);
      Aiger.write_to_file (add i j) file_name
    ) ()
  ) ()
  *)    


(*

let aiger matrix_size int_size = 
  let mA = var "uncontrollable_A" (Type.array (Type.array (Type.int int_size) matrix_size) matrix_size) in
  let mB = var "uncontrollable_B" (Type.array (Type.array (Type.int int_size) matrix_size) matrix_size) in
  let mC = var "controllable_C" (Type.array (Type.array (Type.int int_size) matrix_size) matrix_size) in
  let err = var "err" Type.bool in

  let a i j = get (get mA (int j)) (int i) in
  let b i j = get (get mB (int j)) (int i) in
  let c i j = get (get mC (int j)) (int i) in

  let error i j = 
    let sum = 
      let rec aux accu k = 
	if k = matrix_size then accu
	else 
	  let prod = mult (a i k) (b k j) in
	  aux (add accu prod) (k+1)
      in aux (mult (a i 0) (b 0 j)) 1
    in
    neg (equals (c i j) sum)
  in 

  functional_synthesis [err, error 0 0]


let main = 
  Common.iter 10 10 (fun i () -> 
    Common.iter 10 10 (fun j () -> 
     let file_name = "matrix_mult_"^string_of_int i^"_"^string_of_int j^".aag" in
     Printf.printf "writing aiger to %s\n" file_name;
     Aiger.write_to_file (aiger i j) file_name
     ) ()
  ) ()
 *)
