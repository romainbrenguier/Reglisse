module Aiger = AigerImperative
(** Functions to transform AIGER into verilog code *)
let symbol_to_string s = match s with
  (* | (name,Some i) -> name^"["^string_of_int i^"]" *)
  | (name,Some 0)
  | (name,None) -> name
  | _ -> failwith "in Verilog.symbol_to_string: for now only Boolean values can be used"

let lit2string aiger lit = 
  if lit = Aiger.aiger_true then "1"
  else if lit = Aiger.aiger_false then "0"
  else
    match Aiger.lit2string aiger lit with
    | Some s -> s
    | None -> 
       "(lit "^string_of_int lit^" not found)"



let of_aiger name aiger outch =
  Printf.printf "Writing verilog for aiger:\n";
  Aiger.write stdout aiger;
  Printf.fprintf outch "module %s(\n  input clk,\n" name;
  List.iter
    (fun i -> 
      Printf.fprintf outch "  input %s,\n" i
    ) (Aiger.inputs_exn aiger);

  (match Aiger.outputs_exn aiger with
  | hd :: tl ->
    List.iter
      (fun i -> 
	Printf.fprintf outch "  output %s,\n" i
      ) tl;
    Printf.fprintf outch "  output %s\n" hd
  | _ -> failwith "warning: empty output"
  );
  
  Printf.fprintf outch ");\n";

  List.iter
    (fun i -> 
      Printf.fprintf outch "  reg %s;\n" i
    ) (Aiger.latches_exn aiger);

  Hashtbl.iter
    (fun i (_,_) -> 
      Printf.fprintf outch "  wire gate%d;\n" i
    ) aiger.Aiger.ands;

  Hashtbl.iter
    (fun i (a,b) -> 
      let ga = lit2string aiger a in
      let gb = lit2string aiger b in
      Printf.fprintf outch "  assign gate%d = %s & %s;\n" i ga gb
    ) aiger.Aiger.ands;

  Aiger.LitSet.iter
    (fun l ->
      let o = lit2string aiger l in
      Printf.fprintf outch "  assign %s = gate%d;\n" o l
    ) aiger.Aiger.outputs;

  Printf.fprintf outch "\n  always @(posedge clk) begin\n";

  Hashtbl.iter
    (fun l m -> 
      let lhs = lit2string aiger l in
      let rhs = lit2string aiger m in
      Printf.fprintf outch "    %s <= %s;\n" lhs rhs
    ) aiger.Aiger.latches;

  Printf.fprintf outch "  end\n";
  Printf.fprintf outch "endmodule\n"


