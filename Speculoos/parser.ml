
let lexer = Genlex.make_lexer 
  (Type.keywords @ ["true";"false";"!";"&";"+";"|";"^";"=";"<->";"-->";"+";"-";"*";"/";"mod";"<<";">>";"<=";"<";">";">=";"(";")";";";"if";"then";"else";"<-";"match";"with"; "->";".";"var";":";"init";"updates";"||";"when";"{";"}";"?"])

module Expr = Expression

let next_token stream =
  (match Stream.next stream with
  | Genlex.Kwd x-> "Kwd "^x
  | Genlex.Ident x -> "Ident "^x
  | Genlex.String x -> "String "^x
  | Genlex.Float f -> "Float "^string_of_float f
  | Genlex.Int i -> "Int "^string_of_int i
  | Genlex.Char c -> "Char "^String.make 1 c)
let next_token_short stream =
  (match Stream.next stream with
  | Genlex.Kwd x-> x
  | Genlex.Ident x -> x
  | Genlex.String x -> x
  | Genlex.Float f -> string_of_float f
  | Genlex.Int i -> string_of_int i
  | Genlex.Char c -> String.make 1 c)


let remaining_tokens stream =
  let buf = Buffer.create 100 in
  let rec loop () = 
    Printf.bprintf buf "%s " (next_token_short stream);
    loop ()
  in
  try loop () with _ ->  Buffer.contents buf

let max_level = 11

type declaration =
  { variables : (string * (Type.t * Expression.t)) list;
    constructors : (string * Type.t) list }

let empty = {variables = []; constructors = []}

exception Var_not_found of string

let parse stream =
  let stream = lexer stream in 
  let find_var dec name = 
    try snd (List.assoc name dec.variables)
    with Not_found -> raise (Var_not_found name)
  in

  let add_var dec name typ var = 
    { dec with variables = (name,(typ,var)) :: dec.variables}
  in

  let add_constructors dec stl =
    { dec with constructors = List.rev_append stl dec.constructors}
  in

  let is_declared dec name =
    List.mem_assoc name dec.variables
  in

  let find_type dec name = 
    try fst (List.assoc name dec.variables)
    with Not_found -> failwith ("Unknown type for variable "^name)
  in
  
  let find_type_constructor dec name =
    try List.assoc name dec.constructors
    with Not_found -> failwith ("Unknown type for constructor "^name)
  in    


  let rec parse_expr_remainder dec accu level = 
    if level = 11 then 
      parser 
 | [< 'Genlex.Kwd "?"; t = parse_expr dec level; 'Genlex.Kwd ":"; e = parse_expr dec level >] -> Expr.ite accu t e
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else
      if level = 10 then
      parser 
    | [< 'Genlex.Kwd "-->"; e = parse_expr dec level >] -> Expr.implies accu e
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else if level = 9 then 
      parser
    | [< 'Genlex.Kwd "<->"; e = parse_expr dec level >] -> Expr.equiv accu e
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else if level = 8 then 
      parser
    | [< 'Genlex.Kwd "||"; e = parse_expr dec level >] -> Expr.disj accu e
    | [< 'Genlex.Kwd "^"; e = parse_expr dec level >] -> Expr.xor accu e
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else if level = 7 then 
      parser
    | [< 'Genlex.Kwd "&"; e = parse_expr dec level >] -> Expr.conj accu e
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else if level = 6 then 
      parser
    | [< 'Genlex.Kwd "="; e = parse_expr dec level >] -> Expr.equals accu e
    | [< 'Genlex.Kwd "<="; e = parse_expr dec level >] -> Expr.less_eq accu e
    | [< 'Genlex.Kwd "<"; e = parse_expr dec level >] -> Expr.less accu e
    | [< 'Genlex.Kwd ">"; e = parse_expr dec level >] -> Expr.greater accu e
    | [< 'Genlex.Kwd ">="; e = parse_expr dec level >] -> Expr.greater_eq accu e
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else if level = 5 then 
      parser
    | [< 'Genlex.Kwd "<<"; 'Genlex.Int i >] -> Expr.left_shift accu i
    | [< 'Genlex.Kwd ">>"; 'Genlex.Int i >] -> Expr.right_shift accu i
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else if level = 4 then 
      parser
    | [< 'Genlex.Kwd "+"; e = parse_expr dec level >] -> Expr.add accu e
    | [< 'Genlex.Kwd "-"; e = parse_expr dec level >] -> Expr.minus accu e
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else if level = 3 then 
      parser
    | [< 'Genlex.Kwd "*"; e = parse_expr dec level >] -> Expr.mult accu e
    | [< 'Genlex.Kwd "/"; e = parse_expr dec level >] -> Expr.div accu e
    | [< 'Genlex.Kwd "mod"; e = parse_expr dec level >] -> Expr.modulo accu e
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else if level = 2 then
      parser
    | [< 'Genlex.Kwd "." >] -> parse_field accu dec stream
    | [< 'Genlex.Kwd "["; e = parse_expr dec max_level; 'Genlex.Kwd "]" >] -> Expression.get accu e 
    | [< >] -> 
       let e = parse_expr_remainder dec accu (level - 1) stream in
       if e = accu then e else parse_expr_remainder dec e level stream
    else
      parser [< >] -> accu
			   
  and parse_field expr dec =
    parser 
      (* Warning: we have to add syntax for arrays *)
      | [< 'Genlex.Kwd "("; e = parse_expr dec max_level; 'Genlex.Kwd ")" >] -> Expression.get expr e
      | [< 'Genlex.Ident name >] -> Expression.field expr name

  and parse_after_ident dec name level = 
    if is_declared dec name 
    then parse_expr_remainder dec (find_var dec name) level stream
    else parse_constr dec name stream

  and parse_expr dec level = 
    parser
      (* | [< 'Genlex.Kwd "if"; i = parse_expr dec max_level; 'Genlex.Kwd "then"; t = parse_expr dec 10; 'Genlex.Kwd "else"; e = parse_expr dec max_level; r = parse_expr_remainder dec (Expr.ite i t e) level >] -> r *)
      | [< 'Genlex.Kwd "match"; 'Genlex.Ident name; 'Genlex.Kwd "with"; pat = parse_patterns dec (find_type dec name) (find_var dec name) []; r = parse_expr_remainder dec (Expression.match_with (find_type dec name) (find_var dec name) pat) level >] -> r
(*      | [< 'Genlex.Kwd "next"; 'Genlex.Ident name ; r = parse_expr_remainder dec (Expr.next (find_var dec name)) level >] -> r*)
      | [< 'Genlex.Ident name >] -> parse_after_ident dec name level
      | [< 'Genlex.Kwd "true"; r = parse_expr_remainder dec (Expr.bool true)  level >] -> r
      | [< 'Genlex.Kwd "false"; r = parse_expr_remainder dec (Expr.bool false) level >] -> r
      | [< 'Genlex.Int i ; r = parse_expr_remainder dec  (Expr.int i) level >] -> r
      | [< 'Genlex.Kwd "("; e = parse_expr dec max_level; 'Genlex.Kwd ")"; r = parse_expr_remainder dec e level >] -> r
      | [< 'Genlex.Kwd "!"; e = parse_expr dec 2; r = parse_expr_remainder dec (Expr.neg e) level >] -> r
      | [< 'Genlex.Kwd "{"; r = parse_record dec []; 'Genlex.Kwd "}" >] -> Expression.record r
      | [< 'Genlex.Kwd "["; e = parse_exprs dec []; 'Genlex.Kwd "]" >] -> Expression.array (Array.of_list e)

  and parse_record dec accu = 
    parser
    | [< 'Genlex.Ident name; 'Genlex.Kwd "="; e = parse_expr dec max_level >] -> parse_record_rem dec ((name,e)::accu) stream
  and parse_record_rem dec accu =
    parser | [< 'Genlex.Kwd ";" >] -> parse_record dec accu stream | [< >] -> accu

  and parse_constr dec name = 
    parser
    | [< e = parse_expr dec 0 >] ->Expression.constr (find_type_constructor dec name) name e
    | [< >] -> Expression.constr (find_type_constructor dec name) name Expression.unit

  and parse_exprs dec accu = 
    parser 
    | [< e = parse_expr dec max_level; f = parse_exprs_remainder dec (e :: accu) >] -> f
  and parse_exprs_remainder dec accu = 
    parser
      | [< 'Genlex.Kwd ";"; f = parse_exprs dec accu >] -> f
      | [< >] -> accu


  and parse_patterns dec typ expr accu = 
    let parse_opt = parser | [< 'Genlex.Ident x >] -> Some x | [< >] -> None
    in
    parser 
      | [< 'Genlex.Kwd "|"; 'Genlex.Ident constr; c = parse_opt ; 'Genlex.Kwd "->"; 
	   e = parse_expr (match c with 
	   | Some x -> 
	     let typ,var = Expression.match_case typ expr constr in
	     add_var dec x typ var
	   | None -> dec) 
	  max_level;
	   p = parse_patterns dec typ expr ((constr,(fun x -> e)) :: accu)
	   >] -> p
      | [< >] -> accu
  in
  

  let rec parse_dec accu = 
    parser 
| [< 'Genlex.Kwd "var"; 'Genlex.Ident name; 'Genlex.Kwd ":"; t = Type.parse; 'Genlex.Kwd ";" >] -> 
    parse_dec (add_constructors (add_var accu name t (Expression.var name t)) (List.map (fun (a,b,c) -> (a,c)) (Type.constructors t))) stream
  | [< >] -> accu
  in
  
  let rec parse_updates dec accu = 
    let open Speculoos in
    parser 
    | [< u = parse_expr dec max_level; 'Genlex.Kwd "<-"; e = parse_expr dec max_level; 'Genlex.Kwd ";"; r = parse_updates dec (add_update accu u e) >] -> r
    | [< 'Genlex.Kwd "init"; u = parse_expr dec max_level; 'Genlex.Kwd "<-"; e = parse_expr dec max_level; 'Genlex.Kwd ";"; r = parse_updates dec (add_init accu u e) >] -> r
    | [< 'Genlex.Kwd "when"; e = parse_expr dec max_level; 'Genlex.Kwd "{"; ups = parse_updates dec empty; 'Genlex.Kwd "}"; r  = parse_updates dec (add_when accu e ups) >] -> r

    | [< 'Genlex.Kwd "if"; e = parse_expr dec max_level; 'Genlex.Kwd "then"; 'Genlex.Kwd "{"; ups = parse_updates dec empty; 'Genlex.Kwd "}"; 'Genlex.Kwd "else"; 'Genlex.Kwd "{"; ups2 = parse_updates dec empty; 'Genlex.Kwd "}";r  = parse_updates dec (add_if accu e ups ups2) >] -> r
    | [< >] -> accu
  in
  
  let start_parse_updates dec =
    parser
    | [< 'Genlex.Kwd "updates" >] -> parse_updates dec Speculoos.empty stream
    | [< >] -> parse_updates dec Speculoos.empty stream
  in

  try 
    let dec = parse_dec empty stream in
    let spec = start_parse_updates dec stream in
    Stream.empty stream;
    spec
  with 
  | Stream.Failure ->
    Printf.printf "Remaining: %s\n" (remaining_tokens stream);
    print_newline ();
    Printf.printf "Warning: unexpected token \"%s\" in Parser.parse\n" (next_token stream);
    raise Stream.Failure
  | Stream.Error x ->   
    print_string "Stream error: "; print_endline x;
    Printf.printf "Remaining: %s\n" (remaining_tokens stream);
    Printf.printf "Warning: unexpected token \"%s\" in Parser.parse\n" (next_token stream);
    raise (Stream.Error x)
  | x -> print_endline "unknown error occured"; raise x
	
      
let parse_inch inch =
  let stream = Stream.of_channel inch in
  parse stream

    
    


