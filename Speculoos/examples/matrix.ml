(* ocamlbuild -tag use_ocaml-aiger -tag use_ocaml-cudd examples/matrix.byte -- *)

open Speculog
open Expression

let add = (Aiger.read_from_file "add_2.aag")


let iter_import_module modul renaming size generate =
  let rec aux accu i = 
    if i > size then generate (List.map Array.of_list accu)
    else
      import_module modul (renaming i)
		    (fun l -> 
		     let rec make_list accu = function 
		       | [],[] -> List.rev accu
		       | list, [] -> List.map (fun x -> [x]) list
		       | (hd1::tl1),(hd2::tl2) -> make_list ((hd1::hd2)::accu) (tl1,tl2)
		     in 
		     let new_accu = make_list [] (l,accu) in
		     aux new_accu (i+1)
		    )
  in aux [] 0
  
  
let size_matrix = 4

let main = 
  (*let aiger =
    import_module add      
      [
	"a","a[0][0]";
	"b","b[0][0]";
	"c","c[0][0]";
      ]
      (function [a;b;c] -> 
		let err = var "err" Type.Bool in
		functional_synthesis [err,neg (equals c (int 0))]
      ) 
  in
  let aiger = Aiger.full_hide aiger "c[0][0]" in*)

  let test = iter_import_module add 
	 (fun n -> 
	  let i = n / size_matrix in let j = n mod size_matrix in
	  ["a","a["^string_of_int i^"]["^string_of_int j^"]";
	   "b","b["^string_of_int i^"]["^string_of_int j^"]";
	   "c","c["^string_of_int i^"]["^string_of_int j^"]";])
	 (size_matrix * size_matrix)
	 (function [varA;varB;varC] ->
	  let err = var "err" Type.Bool in 
	  let mC = var "controllable_c" (Type.array (Type.array (Type.int 2) size_matrix) size_matrix) in
	  let getC i j = get (get mC (int i)) (int j) in
	    functional_synthesis
	      [err,for_some [0,size_matrix * size_matrix] 
			    (fun n -> 
			     let i = n / size_matrix in let j = n mod size_matrix in
							neg (equals varC.(i) (getC i j))
			    )
	      ]
	 )
  in

  let rec loop n accu = 
    if n = (size_matrix * size_matrix) then accu 
    else
      let i = n / size_matrix in 
      let j = n mod size_matrix in
      let aig = Aiger.full_hide accu ("c["^string_of_int i^"]["^string_of_int j^"]") in
      loop (n+1) aig
  in
  Aiger.write (loop (size_matrix * size_matrix) test) stdout

