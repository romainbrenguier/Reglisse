type t = int
let compare a b = a - b
let last_variable = ref 0
let table_variable = Hashtbl.create 20
let reverse_table = Hashtbl.create 20

let to_string v = "Variable("^string_of_int v^","^(Hashtbl.find reverse_table v)^")"

let new_variable symbol = 
  let i = !last_variable in
  last_variable := !last_variable + 2;
  Hashtbl.add table_variable symbol i;
  Hashtbl.add reverse_table i symbol;
  i
    
let find l = 
  try Hashtbl.find table_variable l 
  with Not_found -> new_variable l

let to_bdd var = Cudd.ithVar var

let next var = 
  if var mod 2 = 1
  then failwith "variables is already next"
  else var + 1

let make_cube var_list = Cudd.make_cube var_list

let to_int v = v

let of_int v = v

let of_lit_exn aiger lit = 
  find (AigerImperative.lit2string_exn aiger lit)

let max_var () = !last_variable

let rename_configuration bdd variables next_variables =
  Cudd.bddSwapVariables bdd variables next_variables

