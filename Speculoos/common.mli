(** Usefull functions for the size of integers *)
(** [log i] corresponds to the number of bits neccessary to represent [i] in binary. *)
val log : int -> int
(** [exp i] is the number of integer that can be represented using [i] bits. *)
val exp : int -> int

(** Iter a function for the integers within the given bound *)
val iter : int -> int -> (int -> 'a -> 'a) -> 'a -> 'a

(** Functions that are used by several of our modules. *)
val next_token : Genlex.token Stream.t -> string
val remaining_tokens : Genlex.token Stream.t -> string

val display_debug : bool ref
val debug : string -> unit

val display_infos : bool ref
val infos : string -> unit

val display_warnings : bool ref
val warning : string -> unit

(** [starts_with prefix string] tells if [string] starts with prefix [prefix]. *)
val starts_with : string -> string -> bool

val list_to_string : string list -> string -> string

val tmp_name : unit -> string
