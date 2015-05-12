
(** Transform an aiger literal into an string identifier *)
val lit2string : Aiger.t -> Aiger.lit -> string

(** Output an Verilog file describing the same circuit as the given AIG *)
val of_aiger : string -> Aiger.t -> out_channel -> unit

