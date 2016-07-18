(** Extension to regular expression *)

(** Type of extended expressions *)
type t

(** Regular expression *)
val regexp : RegularExpression.t -> t

(** Intersection of two expressions *)
val inter : t -> t -> t

(** Negation of an expression *)
val neg : t -> t

(** Alternative between several expressions*)
val alt : t list -> t

(** Concatenation of two extended expressions *)
val concat : t -> t -> t

(** Conversion to an Aiger cicuit which accept the language corresponding to the expression *)
val to_aiger : ?prefix:string -> t -> AigerImperative.t

(** Parse a regular expression *)
val of_string : string -> t
