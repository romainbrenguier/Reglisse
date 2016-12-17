(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/rising_edge.native --*)
open Expression

let x = var "x" Type.bool 
let previous = var "previous" Type.bool 
let _ = compile 
  [
    Update(Var.bool "rising_edge", bool false); 
    When(neg previous, [Update(Var.bool "rising_edge", x)]); 
    Update(previous, x)
  ]

