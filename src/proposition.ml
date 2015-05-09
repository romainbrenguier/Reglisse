open Expression

type t = 
| Var of string * int
| And of t * t
| Or of t * t
| Not of t
| True | False

let var s i = Var(s,i)
let conj a b = And(a,b)
let disj a b = Or(a,b)
let neg a = Not a
let bool a = if a then True else False

let rec eval valuation = function
  | Var (s,i) -> valuation s = i
  | And (a,b) -> eval valuation a && eval valuation b
  | Or (a,b) -> eval valuation a || eval valuation b
  | Not a -> not (eval valuation a)
  | True -> true
  | False -> false

let rec parse = 
  parser
  | [< a = parse_atomic; r = parse_prop_remainder a >] -> r
  | [< 'Genlex.Kwd "("; e = parse; 'Genlex.Kwd ")"; r = parse_prop_remainder e >] -> r
  | [< 'Genlex.Kwd "!"; e = parse_prop_not; r = parse_prop_remainder e >] -> r
and parse_atomic = 
  parser
    | [< 'Genlex.Ident name >]
      (*'Genlex.Kwd "=" ; 'Genlex.Int i >] *) -> Var(name,1)
    | [< 'Genlex.Kwd "true" >] -> True
    | [< 'Genlex.Kwd "false" >] -> False
and parse_prop_remainder e =
  parser
    | [< 'Genlex.Kwd "|";  f = parse >] -> Or(e,f)
    | [< 'Genlex.Kwd "&";  f = parse >] -> And(e,f)
    | [< 'Genlex.Kwd "<->";  f = parse >] -> Or (And(e,f),And(Not(e),Not(f)))
    | [< >] -> e
and parse_prop_not =
  parser
    | [< 'Genlex.Ident name >]
      (* ; 'Genlex.Kwd "=" ; 'Genlex.Int i >]*) -> (Var(name,0))
    | [< 'Genlex.Kwd "("; e = parse; 'Genlex.Kwd ")" >] -> Not e

let parse_keywords = ["=";"(";")";"!";"|";"&";"<->";"true";"false"]

let rec to_string = function
  | Var (s,i) ->
    if i = 1 then s else if i = 0 then "(!"^s^")" else failwith "comparison with integers is not implemented"
  | And (a,b) -> "("^to_string a ^" & "^to_string b^")"
  | Or (a,b) -> "("^to_string a ^" | "^to_string b^")"
  | Not a -> "(!("^to_string a ^"))"
  | True -> " 1 "
  | False -> " 0 "

let compare a b = compare a b

let rec labels = function
  | Var (s,i) -> Common.StringSet.singleton s
  | And (a,b) -> Common.StringSet.union (labels a) (labels b)
  | Or (a,b) -> Common.StringSet.union (labels a) (labels b)
  | Not a -> labels a
  | True | False -> Common.StringSet.empty

let to_speculog prop = 
  let rec aux = function 
    | Var (s,i) -> 
      if i = 1 then s else if i = 0 then "(!"^s^")" else failwith "comparison with integers is not implemented"
    | And (a,b) -> "("^aux a ^" & "^aux b^")"
    | Or (a,b) -> "("^aux a ^" | "^aux b^")"
    | Not a -> "(!("^aux a ^"))"
    | True -> " 1 "
    | False -> " 0 "
  in aux prop

let to_expression prop inputs = 
  let rec aux = function 
    | And (a,b) -> Expression.conj (aux a) (aux b)
    | Or (a,b) -> Expression.disj (aux a) (aux b)
    | Not a -> Expression.neg (aux a)
    | True -> Expression.bool true
    | False -> Expression.bool false
    | Var (s,i) -> 
      try
	if i = 1 then List.assoc s inputs 
	else if i = 0 then Expression.neg (List.assoc s inputs) else
	  failwith "comparison with integers is not implemented"
      with Not_found -> failwith ("variable "^s^" not found in RegularExpression.Proposition.to_expression "^to_string prop)

  in aux prop
  
let to_bdd prop = 
  let rec aux = function
    | Var (s,i) -> 
	(*Printf.printf "to_bdd: using lit %d for symbol %s\n" (Aiger.lit2int (Aiger.symbol2lit aiger (s,None))) s;*)
      let bdd_of_symbol = AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (s,None))) in
      if i = 1
      then bdd_of_symbol
      else if i = 0
      then Cudd.bddNot bdd_of_symbol
      else failwith "comparison with integers is not implemented"
    | And (a,b) -> Cudd.bddAnd (aux a) (aux b)
    | Or (a,b) ->  Cudd.bddOr (aux a) (aux b)
    | Not a -> Cudd.bddNot (aux a) 
    | True -> Cudd.bddTrue()
    | False -> Cudd.bddFalse()
  in aux prop

let conj a b = match a,b with 
  | True , y | y , True -> y
  | False , _ | _ , False -> False
  | x , y -> And (x,y)

let disj a b = match a,b with 
  | True , _ | _ , True -> True
  | False , y | y , False -> y
  | x , y -> Or (x,y)

let neg a = match a with 
  | True -> False | False -> True | Not x -> x | x -> Not x
