type t = { latch_configuration : Cudd.bdd; latch_input_configuration : Cudd.bdd }
  
let make x y = {latch_configuration=x; latch_input_configuration= y}
let of_bdds x y = make x y

let latch_configuration x = x.latch_configuration 
let latch_input_configuration x = x.latch_input_configuration 
let union x y = make (Cudd.bddOr (latch_configuration x) (latch_configuration y)) (Cudd.bddOr (latch_input_configuration x) (latch_input_configuration y)) 


let equal x y =  
  (Cudd.equal (latch_configuration x) (latch_configuration y))
  && (Cudd.equal (latch_input_configuration x) (latch_input_configuration y))

let ff () = make (Cudd.bddFalse()) (Cudd.bddFalse())
let tt () = make (Cudd.bddTrue()) (Cudd.bddTrue())
let intersection x y = make (Cudd.bddAnd (latch_configuration x) (latch_configuration y)) (Cudd.bddAnd (latch_input_configuration x) (latch_input_configuration y)) 

let intersection_bdd x bdd = make (Cudd.bddAnd (latch_configuration x) bdd) (Cudd.bddAnd (latch_input_configuration x) bdd)

let negation x = make (Cudd.bddNot (latch_configuration x)) (Cudd.bddNot (latch_input_configuration x))
let diff x y = intersection x (negation y)

let rec greatest_fixpoint f x = 
  let next = union (f x) x in
  if equal next x then x else greatest_fixpoint f next

let rec smallest_fixpoint f x = 
  let next = intersection (f x) x in
  if equal next x then x else smallest_fixpoint f next


let bdd_of_lit aiger lit = BddVariable.to_bdd (BddVariable.of_lit_exn aiger lit)

let initial_state aiger = 
  List.fold_left Cudd.bddAnd (Cudd.bddTrue()) 
    (List.map Cudd.bddNot 
       (List.map (bdd_of_lit aiger) 
	  (List.rev_append (AigerImperative.LitSet.elements aiger.AigerImperative.outputs) 
	     (Hashtbl.fold (fun x _ accu -> x :: accu) aiger.AigerImperative.latches []))))

let includes_initial region =
  let m = BddVariable.max_var () in
  let bdd = ref (Cudd.bddTrue()) in
  for i = 0 to m do 
    bdd := Cudd.bddAnd (!bdd) (Cudd.bddNot (Cudd.ithVar i))
  done;
  let includes = Cudd.bddRestrict (latch_configuration region) (!bdd) in
  Cudd.value includes = 1 
