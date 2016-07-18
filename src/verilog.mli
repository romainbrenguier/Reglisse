
(** Transform an aiger literal into an string identifier *)
val lit2string : AigerImperative.t -> AigerImperative.lit -> string

(** Output an Verilog file describing the same circuit as the given AIG *)
val of_aiger : string -> AigerImperative.t -> out_channel -> unit

