
let aiger = 
  input a:s_int 4 in
input b:s_int 4 in
output sum 5 in
output diff 4 in
output product 8 in
output divis 5 in
output modul 5 in
try

SPEC [ 
  sum == (a ++ b); 
  (diff == (a -- b)) or (diff == (b -- a)); 
  product == (mult a b);
  divis == (div a b);
  modul == (modulo a b);
  ]
with NonSynthesizable (x,y) -> Printf.printf "Non Synthesizable: %s\n%s\n" (to_string x) (to_string y); failwith "error."    

let main =
  print_endline "writing file arith.aig";
  Aiger.write_to_file aiger "arith.aig"
