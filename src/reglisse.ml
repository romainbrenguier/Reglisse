(* Transforms regular expression specifications in HDL (Verilog or AIGER) *)

type safety = 
  | Not of string 
  | Never of string 
  | If_then of string * string 
  | Only_if of string * string
  | When of string * string 
  | If_then_else of string * string * string 
  | When_else of string * string * string 

let rec safety_to_expr = function 
  | Not s -> ExtendedExpression.of_string s
  | Never s -> ExtendedExpression.of_string ("{true} * ("^s^")")
  | If_then (if_string, then_string) ->
     let regexp_if = RegularExpression.of_string if_string in
     let regexp_then = Sequence.string_neg then_string in
     let exp = RegularExpression.concat regexp_if regexp_then in
     ExtendedExpression.regexp exp
  | Only_if (if_string, then_string) ->
     let regexp_if = Sequence.string_neg if_string in
     let regexp_then = RegularExpression.of_string then_string in
     let exp = RegularExpression.concat regexp_if regexp_then in
     ExtendedExpression.regexp exp
  | When (s,t) -> 
     safety_to_expr (If_then ("{true} * ("^s^")",t))
  | If_then_else (if_string, then_string, else_string) ->
     let regexp_if = RegularExpression.of_string if_string in
     let regexp_then = Sequence.string_neg then_string in
     let regexp_else = Sequence.string_neg else_string in
     let exp1 = RegularExpression.concat regexp_if regexp_then in
     ExtendedExpression.alt 
       [ ExtendedExpression.regexp exp1;
	 ExtendedExpression.concat 
	   (ExtendedExpression.neg (ExtendedExpression.regexp regexp_if))
	   (ExtendedExpression.regexp regexp_else)]
  | When_else (s,t,e) -> 
     safety_to_expr (If_then_else ("{true} * ("^s^")",t,e))
     

type t =
  { 
    module_name: string;
    inputs: string list; 
    outputs: string list;
    safety: safety list;
    eventually: string list;
    module_calls: (string * string list) list
  }

let new_module name = 
  {module_name=name;inputs=[];outputs=[];safety=[];eventually=[]; module_calls=[]}

let add_input m input = {m with inputs=input :: m.inputs}
let add_output m output = {m with outputs=output :: m.outputs}
let add_safety m never = {m with safety=never :: m.safety}
let add_eventually m eventually = {m with eventually=eventually :: m.eventually}
let add_call m c = { m with module_calls = c :: m.module_calls }


let lexer = Genlex.make_lexer
  ["module";"endmodule";"never";"enventually";
   "only";"if";"then";"else";"input";"output";"when";
   "(";")";",";";"]

exception NoMoreModule

let parse stream =
  let parse_else = parser
  | [< 'Genlex.Kwd "else"; 'Genlex.String regexp; 'Genlex.Kwd ";" >] -> Some regexp
  | [< 'Genlex.Kwd ";" >] -> None
  in

  let rec parse_arguments accu = parser
  | [< 'Genlex.Ident a >] -> (parser
  | [< 'Genlex.Kwd "," >] -> parse_arguments (a :: accu) stream
  | [< 'Genlex.Kwd ")" >] -> a :: accu) stream
  in

  let rec parse_conditions m = parser
  | [< 'Genlex.Kwd "not" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Not regexp)) stream
  | [< 'Genlex.Kwd "never" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Never regexp)) stream
  | [< 'Genlex.Kwd "eventually" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     failwith "eventually not supported";
     parse_conditions (add_eventually m regexp) stream

  | [< 'Genlex.Kwd "if" ; 'Genlex.String regexp; 'Genlex.Kwd "then"; 'Genlex.String seq; x = parse_else >] ->
     (match x with None -> parse_conditions (add_safety m (If_then (regexp,seq))) stream
		| Some e -> parse_conditions (add_safety m (If_then_else (regexp,seq,e))) stream)

  | [< 'Genlex.Kwd "when" ; 'Genlex.String regexp; 'Genlex.Kwd "then"; 'Genlex.String seq; x = parse_else >] ->
     (match x  with None -> parse_conditions (add_safety m (When (regexp,seq))) stream
		  | Some e -> parse_conditions (add_safety m (When_else (regexp,seq,e))) stream)

  | [< 'Genlex.Kwd "only"; 'Genlex.Kwd "if" ; 'Genlex.String seq; 'Genlex.Kwd "then"; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Only_if (seq,regexp))) stream

  | [< 'Genlex.Ident name; 'Genlex.Kwd "("; a = parse_arguments []; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_call m (name, a)) stream

  | [< 'Genlex.Kwd "endmodule" >] -> m
  | [< >] -> failwith "in Reglisse.parse: waiting for [never \"regexp string\";] or [endmodule]"
  in

  let rec parse_arguments m = parser
      | [< 'Genlex.Kwd "input"; 'Genlex.Ident i >] ->
	parse_next_argument (add_input m i) stream
      | [< 'Genlex.Kwd "output"; 'Genlex.Ident o >] ->
	parse_next_argument (add_output m o) stream
      | [< >] -> failwith "in Reglisse.parse: waiting for [input input_name] or [output output_name]"

  and parse_next_argument m = parser
      | [< 'Genlex.Kwd "," >] -> parse_arguments m stream
      | [< 'Genlex.Kwd ")"; 'Genlex.Kwd ";" >] -> parse_conditions m stream
      | [< >] -> failwith "in Reglisse.parse: waiting for [ ); ] or [ , ]"
  in

  let rec aux = parser
| [< 'Genlex.Kwd "module"; 'Genlex.Ident mn; 'Genlex.Kwd "(" >] ->
  parse_arguments (new_module mn) stream
| [< >] -> raise NoMoreModule
  (*failwith "in Reglisse.parse: stream should start with: [module module_name ( ]"*)

  in aux stream

let empty_environment = Hashtbl.create 1

let reglisse_to_aiger ?(prefix="never") ?(environment=empty_environment) t = 
  let aiger1 = 
    if t.safety = [] then None
    else 
      let expressions = List.map safety_to_expr t.safety in
      let expr = ExtendedExpression.alt expressions in
      Some (ExtendedExpression.to_aiger ~prefix expr)
  in
  let aiger2 = 
    if t.eventually = [] then None
    else
      let expressions = List.map RegularExpression.of_string t.eventually in
      let expr = RegularExpression.alt expressions in
      Some (RegularExpression.to_aiger ~prefix:"eventually" expr)
  in
  match aiger1,aiger2 with 
  | Some a, None -> a
  | _ , Some a -> failwith "warning: for now reachability conditions are not working"
  | None, None -> 
     print_endline "compositional module";
     failwith "in Reglisse.reglisse_to_aiger: compositional module not yet implemented"

