open Speculog 
open Expression

let aiger_add i = 
  let a = var "a" (Type.int i) in
  let b = var "b" (Type.int (i+1)) in
  let c = var "c" (Type.int (i+2)) in
  functional_synthesis [c,add a b]


let aiger_mult i = 
  let a = var "a" (Type.int i) in
  let b = var "b" (Type.int i) in
  let c = var "c" (Type.int (2 * i)) in
  functional_synthesis [c,mult a b]

let aiger_complex_mult i = 
  let a = var "a" (Type.record ["real",Type.int i; "imaginary",Type.int i]) in
  let b = var "b" (Type.record ["real",Type.int i; "imaginary",Type.int i]) in
  let c = var "c" (Type.record ["real",Type.int (2*i); "imaginary",Type.int (2*i)]) in
  let real x = field x "real" in let complex x = field x "imaginary" in
  functional_synthesis 
    [
      real c, minus (mult (real a) (real b)) (mult (complex a) (complex b));
      complex c, add (mult (real a) (complex b)) (mult (complex a) (real b));
    ]


let main =
  (*
  Common.iter 2 20 (fun i () ->
  let file_name = "add_"^string_of_int i^".aag" in
  print_endline ("writing aiger to "^file_name);
  Aiger.write_to_file (aiger_add i) file_name
		   )();
  Common.iter 1 8 (fun i () ->
  let file_name = "mult_"^string_of_int i^".aag" in
  print_endline ("writing aiger to "^file_name);
  Aiger.write_to_file (aiger_mult i) file_name
		   )();
  *)
  Common.iter 1 8 (fun i () ->
  let file_name = "mult_complex_"^string_of_int i^".aag" in
  print_endline ("writing aiger to "^file_name);
  Aiger.write_to_file (aiger_complex_mult i) file_name
		   )();

