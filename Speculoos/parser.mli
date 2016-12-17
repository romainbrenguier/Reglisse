
(* The first list contains the initial condition and the second one the updates *)
val parse : char Stream.t -> Speculoos.t

val parse_inch : in_channel -> Speculoos.t

val remaining_tokens : Genlex.token Stream.t -> string
