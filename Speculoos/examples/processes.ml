(* ocamlbuild -tag use_ocaml-aiger -tag use_ocaml-cudd processes.byte *)
module StringMap = Map.Make(String)

module Instr = 
struct
  type t = 
  | Call of string
  | Assign of Expression.t * Expression.t
  | Assert of Expression.t
  | Synth
      
  let to_string = function
    | Call (name) -> name^"();"
    | Assign (e,f) -> Expression.to_string e ^" = "^ Expression.to_string f^";"
    | Assert e -> "assert ("^Expression.to_string e^");"
end

type t = Instr.t array

type env = t StringMap.t

let id_of_procedure env proc = 
  match StringMap.fold (fun s _ (i,res) -> if s = proc then (i+1,Some i) else (i+1,res)) env (0,None) with
  | _,None -> raise Not_found
  | _,Some i -> i

let type_position nb_procedures length =
  Type.record
    ["procedure",Type.int (Common.log (nb_procedures+1));
     "position",Type.int (Common.log length);
     "main_position",Type.int (Common.log length);
    ]

let type_var = Type.int 8

let type_action nb_procedures length =
  Type.union ["call",Type.int (Common.log (nb_procedures+1));
	      "void",Type.unit]

let to_expr env instructions = 
  let position = Expression.var "position" (type_position (StringMap.cardinal env) 10)
  in
  let action = Expression.var "action" (type_action  (StringMap.cardinal env) 10) 
  in
  Common.iter 0 (Array.length instructions - 1)
    (fun i accu -> 
      match instructions.(i) with
      | Instr.Assign (e,f) -> (e,f) :: accu
      | Instr.Call s -> 
	(Expression.field position "procedure", Expression.int (id_of_procedure env s)) ::
	  (Expression.field position "position",Expression.int 0) ::
	  accu
    ) [] 

let main = 
  let e = to_expr StringMap.empty [| Assign (Expression.var "x" (Type.int 4) ,Expression.int 2)|] in
  List.iter
    (fun (e,f) -> 
      Printf.printf "%s <- %s;\n" (Expression.to_string e) 
	(Expression.to_string f)
    ) e
