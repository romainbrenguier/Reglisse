(** Type of regular expressions *)
type t = 
| Prop of Proposition.t 
| Alt of t list
| Concat of t * t
| Star of t
| Epsilon
| Empty

(** The grammar for regular expression is given by : 
r ::= r | r ; r & r ; r* ; r r; (r); { p }; r+ ; r?; .
p ::= p | p ; p & p ; !p ; p <-> p; (p); ident ; true
*)
val of_string : string -> t

(** Alternative between two expressions *)
val disj : t -> t -> t
val alt : t list -> t
(** Concatenation *)
val concat : t -> t -> t
val seq : t list -> t
(** Repetition *)
val star : t -> t
val plus : t -> t
(** [times e n] repeat [n] times expression [e] *)
val times : t -> int -> t
(** Optional expression *)
val opt : t -> t
(** Proposition *)
val prop : Proposition.t -> t

(** Labels appearing in the expression *)
val labels : t -> Common.StringSet.t

val to_string : t -> string

(** [to_aiger] and [regexp expr] convert a regular expression 
    into an AIG with an output called ACCEPT that is true after seeing 
    an accepting state and a variable RESIDUAL that encodes
    the current state.
*)
val to_aiger : ?prefix:string -> t -> Aiger.t
val regexp : string -> Aiger.t

val test : string -> unit
