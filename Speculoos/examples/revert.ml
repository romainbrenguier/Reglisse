(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/revert.native --*)
open Expression

let _ = compile [Update(Var.int "y" 8, select (Var.int "x" 8) [3,0;7,4])]
