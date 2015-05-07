(** Functions to transform AIGER into verilog code *)
let symbol_to_string s = match s with
  | (name,Some i) -> name^"__"^string_of_int i
  | (name,None) -> name

let lit2string aiger lit = 
  if lit = Aiger.aiger_true then "1"
  else if lit = Aiger.aiger_false then "0"
  else
    try 
      let string = 
	match Aiger.lit2tag aiger (Aiger.strip lit) with
	| Aiger.Constant true -> "1" 
	| Aiger.Constant false -> "0"
	| Aiger.And (l,_,_) -> "gate"^string_of_int (Aiger.lit2int l)
	| _ -> symbol_to_string (Aiger.lit2symbol aiger (Aiger.strip lit))
      in
      if Aiger.sign lit then "(!"^string^")" else string
    with Not_found -> "(lit "^string_of_int (Aiger.lit2int lit)^" not found)"

let of_aiger aiger outch =
  Printf.fprintf outch "module FromAiger(\n  input clk,\n";
  
  List.iter
    (fun i -> 
      Printf.fprintf outch "  input %s,\n" i
    ) (Aiger.inputs aiger);

  (match Aiger.outputs aiger with
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
    ) (Aiger.latches aiger);

  List.iter
    (fun (i,_,_) -> 
      Printf.fprintf outch "  wire gate%d;\n" (Aiger.lit2int i)
    ) aiger.Aiger.ands;

  List.iter
    (fun (i,a,b) -> 
      let ga = lit2string aiger a in
      let gb = lit2string aiger b in
      Printf.fprintf outch "  assign gate%d = %s & %s;\n" (Aiger.lit2int i) ga gb
    ) aiger.Aiger.ands;

  Printf.fprintf outch "\n  always @(posedge clk) begin\n";

  List.iter
    (fun (l,m) -> 
      let symbol = Aiger.lit2symbol aiger l in
      let gm = lit2string aiger m in
      Printf.fprintf outch "    %s <= %s;\n" (symbol_to_string symbol) gm
    ) aiger.Aiger.latches;

  List.iter
    (fun o -> 
      let l = Aiger.symbol2lit aiger (o,None) in
      let gm = lit2string aiger l in
      Printf.fprintf outch "    %s = %s;\n" o gm
    ) (Aiger.outputs aiger);

  Printf.fprintf outch "  end\n";
  Printf.fprintf outch "endmodule\n"


