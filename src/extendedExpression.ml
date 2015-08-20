(** Extension to regular expression *)

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
(* add a copy of aiger2 in aiger1, changing the literal of aiger2 to point to an literal of aiger1 *)
let replace_lit_by_output aiger1 lit1 aiger2 lit2 =
  let aig = aiger1 in
  let mapping = Hashtbl.create aiger2.Aiger.maxvar in
  let add_mapping lit nlit = 
    Hashtbl.replace mapping lit nlit in
  let find_mapping lit = 
    if Aiger.sign lit 
    then Aiger.aiger_not (Hashtbl.find mapping (Aiger.aiger_not lit))
    else Hashtbl.find mapping lit 
  in

  add_mapping Aiger.aiger_false Aiger.aiger_false;
  add_mapping Aiger.aiger_true Aiger.aiger_true;


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
  aig

let rec to_aiger ?(prefix="") = function 
  | Regexp r -> RegularExpression.to_aiger ~prefix r 

  | Inter (a,b) ->
     let id1 = prefix^new_id () in let id2 = prefix^new_id () in
     let aig1 = to_aiger ~prefix:id1 a in
     let aig2 = to_aiger ~prefix:id2 b in
     let aig = Aiger.compose aig1 aig2 in
     let g1 = Aiger.symbol2lit aig (id1^"_accept",Some 0) in
     let g2 = Aiger.symbol2lit aig (id2^"_accept",Some 0) in
     let aig, v = Aiger.new_var aig in
     let lit = Aiger.var2lit v in
     let aig = Aiger.add_and aig lit g1 g2 in
     let aig = Aiger.add_output aig lit (prefix^"_accept",Some 0) in
     let aig = Aiger.hide aig (id1^"_accept",Some 0) in
     let aig = Aiger.hide aig (id2^"_accept",Some 0) in
     aig

  | Neg a ->
     let id1 = prefix^new_id () in
     let aig1 = to_aiger ~prefix:id1 a in
     let g1 = Aiger.symbol2lit aig1 (id1^"_accept",Some 0) in
     let aig = Aiger.add_output aig1 (Aiger.aiger_not g1) (prefix^"_accept",Some 0) in
     let aig = Aiger.hide aig (id1^"_accept",Some 0) in
     aig
				  
  | Alt (hd::tl) ->
     let id = prefix^new_id () in
     let aig = to_aiger ~prefix:id hd in
     let aig,id =
       List.fold_left
	 (fun (aig,id1) expr ->
	  let id2 = prefix^new_id () in
	  let aig2 = to_aiger ~prefix:id2 expr in
	  let aig = Aiger.compose aig aig2 in
	  let g1 = Aiger.symbol2lit aig (id1^"_accept",Some 0) in
	  let g2 = Aiger.symbol2lit aig (id2^"_accept",Some 0) in
	  let aig, v = Aiger.new_var aig in
	  let lit = Aiger.var2lit v in
	  let aig = Aiger.add_and aig lit (Aiger.aiger_not g1) (Aiger.aiger_not g2) in
	  let id3 = prefix^new_id () in
	  let aig = Aiger.add_output aig (Aiger.aiger_not lit) (id3^"_accept",Some 0) in
	  let aig = Aiger.hide aig (id1^"_accept",Some 0) in
	  let aig = Aiger.hide aig (id2^"_accept",Some 0) in
	  aig, id3
	 ) (aig,id) tl
     in 
     
     let aig = Aiger.rename aig [(id^"_accept",Some 0), (prefix^"_accept",Some 0)] in
     aig 

  | Alt [] -> 
     failwith "in ExtendedExpression.to_aiger: empty alternatives"

  | Concat (a,b) ->
     let id1 = prefix^new_id () in
     let aig1 = to_aiger ~prefix:id1 a in
     let id2 = prefix^new_id () in
     let aig2 = to_aiger ~prefix:id2 b in 
     let lit1 = Aiger.symbol2lit aig1 (id1^"_accept",Some 0) in 
     let lit2 = Aiger.symbol2lit aig2 (id2^"_init",Some 0) in
     let aig = replace_lit_by_output aig1 lit1 aig2 lit2 in
     let aig = Aiger.hide aig (id1^"_accept",Some 0) in
     let aig = Aiger.rename aig [(id2^"_accept",Some 0),(prefix^"_accept",Some 0)] in
     aig

