open Speculog open Expression

let add n = 
  let a = var "a" (Type.int n) in
  let b = var "b" (Type.int (n+1)) in
  let c = var "c" (Type.int (n+2)) in
  functional_synthesis [c, add a b]

let mult n = 
  let a = var "a" (Type.int n) in
  let b = var "b" (Type.int n) in
  let c = var "c" (Type.int (2 * n)) in
  functional_synthesis [c, mult a b]

let main = 
  Common.iter 1 20 (fun i () -> 
    let file_name = "matrix/add"^string_of_int i^".aag" in
    print_endline ("writing aiger to "^file_name);
    Aiger.write_to_file (add i) file_name
  ) ();
  Common.iter 1 4 (fun i () -> 
    let file_name = "matrix/mult"^string_of_int i^".aag" in
    print_endline ("writing aiger to "^file_name);
    Aiger.write_to_file (mult i) file_name
  ) ()
