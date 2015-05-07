type t = Unit | Bool | Int of int | Array of t * int | Record of (string * t) list | Union of (string * t) list

let unit = Unit
let bool = Bool
let int i = Int i
let array t i = Array(t,i)
let record stl = Record stl
let union stl = Union stl


let rec to_string = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int i -> Printf.sprintf "int %d" i
  | Array (t,i) -> Printf.sprintf "%s[%d]" (to_string t) i
  | Record stl -> 
    let buf = Buffer.create 100 in
    Printf.bprintf buf "{ ";
    List.iter 
      (fun (s,t) ->
	Printf.bprintf buf "%s:%s; " s (to_string t)
      ) stl;
    Printf.bprintf buf "}";
    Buffer.contents buf
  | Union ((hds,hdt) :: tl) -> 
    let buf = Buffer.create 100 in
    Printf.bprintf buf "%s of %s" hds (to_string hdt);
    List.iter 
      (fun (s,t) ->
	Printf.bprintf buf " | %s of %s; " s (to_string t)
      ) tl;
    Buffer.contents buf
  | Union [] -> failwith "In Type.to_string : empty union type"

let keywords = ["bool";"int";"unit";":";";";"[";"]";"{";"}";"of";"|";"(";")"]
let lexer = Genlex.make_lexer keywords
let parse = 
  let rec parse_type = 
    parser 
  | [< 'Genlex.Kwd "{"; t = parse_tuple []; 'Genlex.Kwd "}"; p = parse_remainder (Record t) >] -> p
  | [< 'Genlex.Kwd "int"; 'Genlex.Int i; p = parse_remainder (Int i) >] -> p
  | [< 'Genlex.Kwd "bool"; p = parse_remainder Bool >] -> p
  | [< 'Genlex.Kwd "("; t = parse_type; 'Genlex.Kwd ")"; p = parse_remainder t >] -> p
  | [< 'Genlex.Ident name; p = parse_constructor_remaining name; c=parse_constructors [p] >] -> c

  and parse_constructor_remaining name =
    parser
      | [< 'Genlex.Kwd "of"; t = parse_type >] -> name,t
      | [< >] -> name , Unit

  and parse_constructors accu = 
    parser
      | [< 'Genlex.Kwd "|"; 'Genlex.Ident name; p = parse_constructor_remaining name ; r = parse_constructors (p :: accu) >] -> r
      | [< >] -> Union accu

  and parse_tuple accu =
    parser
  | [< 'Genlex.Ident name; 'Genlex.Kwd ":"; t = parse_type; p = parse_tuple_remaining ((name,t) :: accu) >] -> p
  | [< >] -> raise (Stream.Error "expecting a lowercase identifier")

  and parse_tuple_remaining accu =
    parser
      | [< 'Genlex.Kwd ";"; p = parse_tuple accu >] -> p
      | [< >] -> List.rev accu

  and parse_remainder accu =
    parser 
      | [< 'Genlex.Kwd "["; 'Genlex.Int i; 'Genlex.Kwd "]"; p= parse_remainder ( Array (accu,i)) >] -> p
      | [< >] -> accu
  in parse_type 

let of_string st = parse (lexer (Stream.of_string st))

let rec union_sizes = function
  | Union stl ->
     (Common.log (List.length stl - 1)),
     List.fold_left (fun m (_,t) -> max (size t) m) 0 stl
  | t -> failwith ("In Type.union_sizes: type "^to_string t^" is not a union type")
		  
and size = function
  | Unit -> 0
  | Bool -> 1
  | Int i -> i
  | Array (t,i) -> i * (size t)
  | Record stl -> 
    List.fold_left (fun total (_,t) -> size t + total) 0 stl
  | Union stl ->
     let cs,ct = union_sizes (Union stl) in cs+ct

let constructors = 
  let rec aux accu = function
  | Array (t,i) -> aux accu t
  | Record stl ->
    List.fold_left (fun accu (_,t) -> aux accu t) accu stl
  | Union stl -> 
     List.fold_left (fun accu (s,t) -> aux ((s,t,Union stl) :: accu) t) accu stl
  | _ -> accu
  in aux []
