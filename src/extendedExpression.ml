(** Extension to regular expression *)
module Aiger = AigerImperative

type t = 
  | Regexp of RegularExpression.t
  | Inter of t * t
  | Alt of t list
  | Neg of t
  | Concat of t * t
		    
let regexp a = Regexp a
let inter a b = Inter (a,b)
let alt a = Alt a
let neg a = Neg a
let concat a b = Concat (a,b)
let of_string s = regexp (RegularExpression.of_string s)

let id_cnt = ref 0
let new_id () = incr id_cnt; "_ext"^string_of_int !id_cnt

(* usefull for concatenation *)
(* add a copy of aiger2 in aiger1, changing the literal of aiger2 to point to a literal of aiger1 *)
let replace_lit_by_output aiger1 lit1 aiger2 lit2 =
  let aig = aiger1 in
  let mapping = Hashtbl.create aiger2.Aiger.maxvar in
  let add_mapping lit nlit = 
    Hashtbl.replace mapping lit nlit in
  let find_mapping lit = 
    (*if Aiger.sign lit 
    then Aiger.aiger_not (Hashtbl.find mapping (Aiger.aiger_not lit))
    else*) Hashtbl.find mapping lit 
  in

  add_mapping Aiger.aiger_false Aiger.aiger_false;
  add_mapping Aiger.aiger_true Aiger.aiger_true;

  Aiger.LitSet.iter
      (fun lit2 -> 
	let sym = Aiger.lit2string_exn aiger2 lit2 in
	try 
	  let lit1 = Aiger.string2lit_exn aiger1 sym in
	  add_mapping lit2 lit1
	with Not_found -> 
	  let lit = Aiger.add_input aig sym in
	  add_mapping lit2 lit
      ) aiger2.Aiger.inputs;
  
  Hashtbl.iter
      (fun lhs _ -> 
	let sym = Aiger.lit2string_exn aiger2 lhs in
	let lit = Aiger.add_latch aig sym in
	add_mapping lhs lit
      ) aiger2.Aiger.latches;

  Hashtbl.iter 
      (fun lhs (rhs0,rhs1) -> 
	let lit = Aiger.conj aig (find_mapping rhs0) (find_mapping rhs1) in
	add_mapping lhs lit
      ) aiger2.Aiger.ands;

  Hashtbl.iter
    (fun lhs rhs -> 
      Aiger.set_latch_update aig (find_mapping lhs) (find_mapping rhs) 
    ) aiger2.Aiger.latches;

  Aiger.LitSet.iter 
    (fun o -> 
      let sym = Aiger.lit2string_exn aiger2 o in
      Aiger.set_output aig sym (find_mapping o)
    ) aiger2.Aiger.outputs;
  aig

(*
  let aig = 
    List.fold_left 
      (fun aig lit2 -> 
       let sym = Aiger.lit2symbol aiger2 lit2 in
       try 
	 let lit1 = Aiger.symbol2lit aiger1 sym in
	 add_mapping lit2 lit1;
	 aig
       with Not_found -> 
	 let aig,v = Aiger.new_var aig in
	 let lit = Aiger.var2lit v in
	 add_mapping lit2 lit;
	 Aiger.add_input aig lit sym
      ) aig aiger2.Aiger.inputs
  in

  let aig = 
    List.fold_left 
      (fun aig (lhs,_) -> 
       let aig,v = Aiger.new_var aig in
       let lit = Aiger.var2lit v in
       add_mapping lhs lit;
       aig
      ) aig aiger2.Aiger.latches
  in

  let aig = 
    List.fold_left 
      (fun aig (lhs,rhs0,rhs1) -> 
       let aig,v = Aiger.new_var aig in
       let lit = Aiger.var2lit v in
       add_mapping lhs lit;
       Aiger.add_and aig lit (find_mapping rhs0) (find_mapping rhs1)
      ) aig aiger2.Aiger.ands
  in

  let aig = 
    List.fold_left 
      (fun aig (lhs,rhs) -> 
       let sym = Aiger.lit2symbol aiger2 lhs in
       let lit = find_mapping lhs in
       let lit1 = find_mapping rhs in
       Aiger.add_latch aig lit lit1 sym
      ) aig aiger2.Aiger.latches
  in

  let aig = 
    List.fold_left 
      (fun aig o -> 
       let sym = Aiger.lit2symbol aiger2 o in
       Aiger.add_output aig (find_mapping o) sym
      ) aig aiger2.Aiger.outputs
  in

  let aig = AigerBdd.reorder_aiger aig in
  aig*)

let rec to_aiger ?(prefix="") = function 
  | Regexp r -> RegularExpression.to_aiger ~prefix r 

  | Inter (a,b) ->
     let id1 = prefix^new_id () in let id2 = prefix^new_id () in
     let aig1 = to_aiger ~prefix:id1 a in
     let aig2 = to_aiger ~prefix:id2 b in
     let aig = Aiger.compose aig1 aig2 in
     let g1 = Aiger.string2lit_exn aig (id1^"_accept") in
     let g2 = Aiger.string2lit_exn aig (id2^"_accept") in
     let lit = Aiger.conj aig g1 g2 in
     Aiger.set_output aig (prefix^"_accept") lit;
     Aiger.hide_exn aig (id1^"_accept");
     Aiger.hide_exn aig (id2^"_accept");
     aig

  | Neg a ->
     let id1 = prefix^new_id () in
     let aig1 = to_aiger ~prefix:id1 a in
     let g1 = Aiger.string2lit_exn aig1 (id1^"_accept") in
     Aiger.set_output aig1 (prefix^"_accept") (Aiger.neg g1);
     Aiger.hide_exn aig1 (id1^"_accept");
     aig1
				  
  | Alt (hd::tl) ->
     let id = prefix^new_id () in
     let aig = to_aiger ~prefix:id hd in
     let aig,id =
       List.fold_left
	 (fun (aig,id1) expr ->
	  let id2 = prefix^new_id () in
	  let aig2 = to_aiger ~prefix:id2 expr in
	  let aig = Aiger.compose aig aig2 in
	  let g1 = Aiger.string2lit_exn aig (id1^"_accept") in
	  let g2 = Aiger.string2lit_exn aig (id2^"_accept") in
	  let lit = Aiger.conj aig (Aiger.neg g1) (Aiger.neg g2) in
	  let id3 = prefix^new_id () in
	  Aiger.set_output aig (id3^"_accept") (Aiger.neg lit);
	  Aiger.hide_exn aig (id1^"_accept");
	  Aiger.hide_exn aig (id2^"_accept");
	  aig, id3
	 ) (aig,id) tl
     in 
     
     Aiger.rename aig (fun x -> if x = (id^"_accept") then (prefix^"_accept") else x);
     aig 

  | Alt [] -> 
     failwith "in ExtendedExpression.to_aiger: empty alternatives"

  | Concat (a,b) ->
     let id1 = prefix^new_id () in
     let aig1 = to_aiger ~prefix:id1 a in
     let id2 = prefix^new_id () in
     let aig2 = to_aiger ~prefix:id2 b in 
     let lit1 = Aiger.string2lit_exn aig1 (id1^"_accept") in 
     let lit2 = Aiger.string2lit_exn aig2 (id2^"_init") in
     let aig = replace_lit_by_output aig1 lit1 aig2 lit2 in
     Aiger.hide_exn aig (id1^"_accept");
     Aiger.rename aig (fun x -> if x = (id2^"_accept") then (prefix^"_accept") else x);
     aig

