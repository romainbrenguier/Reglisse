open Expression

let cell_type = Type.of_string "{ carry_in : bool; value : bool; carry_out : bool }"

let value x = field x "value"
let carry_in x = field x "carry_in"
let carry_out x = field x "carry_out"


let counter_cell cin =
  [(value b , apply Expr.xor (value b) cin);
   (carry_out b, apply Expr.xor (value b) (carry_in b))]


let main = 
  let bit_list = List.map (fun x -> var x cell_type) ["bit0";"bit1";"bit2"] in
  List.fold_left 
    (fun accu b -> 
    ) [] bit_list
let aig = functional_synthesis counter_cell in
Aiger.write aig stdout
  
