(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/elevator.byte -- *)
open Speculog
open Expression
open RegularExpression

(* [i] identifies the stairs, and [j] the elevator *)
let spec i j = 
  alt
    (seq
       [
	 of_string "{true}*";
	 of_string "{push_i}";
	 times (of_string "{! controllable_pos_1_i & ! controllable_pos_2_i & ! controllable_pos_3_i}") 5;
    ])
    (seq
       [
	 of_string "{true}*";
	 of_string "{push_j_i}";
	 times (of_string "{! controllable_pos_j_i }") 5;
    ])

let main = 
  let sp = spec 1 2 in
  Common.display_debug := true;
  let aig = to_aiger sp in
  Aiger.write aig stdout
