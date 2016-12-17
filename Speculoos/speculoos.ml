open Expression

let var = var
let unit = unit
let bool = bool 
let int = int
let array = array
let record = record
let constr = constr

let neg = neg
let ($=>) = implies
let ($<=>) = equiv
let ($&) = conj
let ($|) = disj
let ($^) = xor
let ($=) = equals
let ($<=) = less_eq
let ($<) = less
let ($>=) = greater_eq
let ($>) = greater
let ( $<< ) = left_shift
let ( $>> ) = right_shift
let ($+) = add
let ($-) = minus
let ( $* ) = mult
let ( $/ ) = div
let ( $% ) = modulo
let ite = ite
let ($?) i (t,e) = ite i t e

let mux c x = mux c x
let andR = andR
let orR = orR
let xorR = xorR

module Syntax = 
struct
  let (=>) = implies
  let (<=>) = equiv
  let ( & ) = conj
  let ( || ) = disj
  let ( ^ ) = xor
  let (==) = equals
  let (<=) = less_eq
  let (<) = less
  let (>=) = greater_eq
  let (>) = greater
  let ( << ) = left_shift
  let ( >> ) = right_shift
  let (+) = add
  let (-) = minus
  let ( * ) = mult
  let ( / ) = div
  let ( % ) = modulo
end


type t = 
| Update of (Expression.t * Expression.t)
| When of (Expression.t * t)
| If of (Expression.t * t * t)
| Init of (Expression.t * Expression.t)
| Seq of t list

let seq t = Seq t
let empty = seq []
let add t x = match t with 
  | Seq l -> Seq (List.rev (x :: (List.rev l)))
  | y -> Seq [y;x]

let add_update t x u = add t (Update(x,u))
let add_when t c a = add t (When(c,a))
let add_if t c a b = add t (If(c,a,b))
let add_init t a b = add t (Init(a,b))
  
let extract_init = 
  let rec aux accu = function
    | Update _ | When (_,_) | If (_,_,_) -> accu
    | Seq tlist -> List.fold_left aux accu tlist
    | Init (a,b) -> (a,b) :: accu
  in aux []


let initialize initial updates =
  (*let decl, initialized = reg "initialized" Bool in*)
  (*
  let rec aux accu = function 
    | Update(var,up) ->
      let i = 
	try List.assoc var initial
	with Not_found -> bool false
      in Update(var, ite initialized up i) :: accu
    | When(t, e) -> When(t, aux e) :: accu
    | If(t, e , f) -> ...
      If(t, List.rev (List.fold_left aux [] tlist), List.rev (List.fold_left aux [] tlist2)) :: accu
    | Seq tlist -> List.rev (List.fold_left aux [] tlist)
  in*)
  let initialized = var "initialized" Type.Bool in
  Seq [Update(initialized,bool true);When(initialized,updates)] 
    


let functional_synthesis synthesis_function instructions =
  let tab = Hashtbl.create 100 in
  let add_when condition x up = 
    try Hashtbl.replace tab x (ite condition up (Hashtbl.find tab x))
    with Not_found -> 
      Hashtbl.add tab x (ite condition up (bool false))
  in

  let rec aux condition = function
    | Seq list -> List.iter (aux condition) list
    | When (a,b) -> aux (a $& condition) b
    | If (e,a,b) -> aux (e $& condition) a;
      aux ((neg e) $& condition) b
    | Init _ -> ()
    | Update (a,b) -> 
      
      let rec aux_update = 
	let open Expression in function
	| EUnit , EUnit -> ()
	| EInt x, EInt y  -> add_when condition x (EInt y)
	| EBool x, EInt y -> add_when condition (Integer.of_boolean x) (EInt y)
	| EBool x, EBool y -> add_when condition (Integer.of_boolean x) (EBool y)
	| EInt x, EBool y -> add_when condition x (EBool y)
      (* (x,y) :: accu*)
	| EArray arr1, EArray arr2 ->
	  Array.iteri (fun i t -> aux_update (t,arr2.(i))) arr1
	| ERecord stl1, ERecord stl2 ->
	  List.iter (fun (s,t) -> aux_update (t, List.assoc s stl2))  stl1
	| _ -> failwith "In Expression.functional_synthesis: the two elements are not of the same type"
      in aux_update (a,b)
  in

  let finalize = function 
    | x, EInt y -> x,y 
    | x, EBool y -> x,Integer.of_boolean y 
    | _ -> failwith "in Speculoos.functional_synthesis: updates contain non basic (int or bool) values"
  in

  aux (bool true) instructions;
  let list = Hashtbl.fold (fun x up accu -> (x,up) :: accu) tab [] in
  synthesis_function (List.map finalize list)

let to_aiger instructions = 
  let inits = extract_init instructions in
  let ups = 
    if inits <> [] 
    then initialize inits instructions 
    else instructions
  in functional_synthesis Synthesis.functional_synthesis ups

let to_aig_imp instructions = 
  let inits = extract_init instructions in
  let ups = 
    if inits <> [] 
    then initialize inits instructions 
    else instructions
  in
  let aig = functional_synthesis SynthesisImp.functional_synthesis ups in
  aig

let compile ?(filename="") a =
  let aig = to_aiger a in
  if filename = "" then Aiger.write aig stdout 
  else (let outch = open_out filename in Aiger.write aig outch; close_out outch)

let to_symbols aiger t = 
  let rec aux = function
    | EUnit -> []
    | EBool (Boolean.EVar (s)) -> [s]
    | EBool _ -> failwith "in Speculoos.to_symbols: the value is not a Boolean variable"
    | EInt i -> 
      let array = Integer.to_boolean_array i in
      Array.fold_left
	(fun accu b -> match b with 
	| Boolean.EVar s -> s :: accu
	| _ -> failwith "in Speculoos.to_symbols: the value contains an expressions which is not a Boolean variable"
	) [] array
    | EArray a -> 
      Array.fold_left (fun accu c -> List.rev_append (aux c) accu) [] a
    | ERecord a ->
      List.fold_left (fun accu (_,c) -> List.rev_append (aux c) accu) [] a
  in aux t

let rename aiger s typ name =
  let u = var s typ in
  let v = var name typ in
  let s1,s2 = to_symbols aiger u, to_symbols aiger v in
  let renaming = List.combine s1 s2 in
  let rename x = try List.assoc x renaming with Not_found -> x in
  AigerImperative.rename aiger rename

let hiding aiger s typ = 
  let u = var s typ in
  let s = to_symbols aiger u in
  List.iter (AigerImperative.hide aiger) s

(*
let use_module aiger ~inputs ~outputs generate =
  let aiger = 
    List.fold_left (fun aig (s,t) -> 
      let name = Common.tmp_name () in
      let typ = to_type t in
      let new_var = var name typ in
      let input = functional_synthesis (Update(new_var, t)) in
      let renamed = rename aig s typ name in
      let aig = Aiger.compose input renamed in
      hiding aig name typ
    ) aiger inputs 
  in
  let aiger = Aiger.full_rename aiger outputs (*input_renaming*) in
  let var_list = 
    List.fold_left 
      (fun accu (r,s) -> 
	(var s (Type.int (Aiger.size_symbol aiger s))) :: accu
      ) [] outputs
  in
  let gen_aiger = (*functional_synthesis*) (generate var_list) in
  let composed = Aiger.compose aiger gen_aiger in
  (*let hiden = List.fold_left (fun aig (r,s) -> Aiger.full_hide aig s) composed outputs in*)
  composed
*)
