(*
 * Copyright 2014 Romain Brenguier
 * Author: Romain Brenguier <romain.brenguier@ulb.ac.be>
 * 
 * This file is part of Ocaml-aiger.
 * 
 * Ocaml-aiger is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details. 
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)



(** An Imperative implementation of the OCaml library for AIGER files *)

type lit = int

val aiger_false : lit
val aiger_true : lit
val neg : lit -> lit

module LitSet :
sig
  type t
  val make : unit -> t
  val fold : (int -> lit -> 'a -> 'a) -> t -> 'a -> 'a
  val add : t -> lit -> unit
  val elements : t -> lit list 
  val iter : (lit -> unit) -> t -> unit
end


(** Type for AIG representation.
    The invariant are that: 
    a litteral represents a unique input latch or and gate.
    the number of entries in [inputs], [latches], [ands] and [outputs] are equal to
    [num_inputs], [num_latches], [num_outputs] and [num_ands] respectively.
*)
type t = {
  mutable maxvar:int;   
  mutable num_inputs:int;
  mutable num_latches:int;
  mutable num_outputs:int;
  mutable num_ands:int;

  inputs: LitSet.t;
  latches:(lit,lit) Hashtbl.t;
  outputs:LitSet.t;
  ands: (lit,lit*lit) Hashtbl.t;
  ands_inv: (lit*lit,lit) Hashtbl.t;
  symbols: (lit,string) Hashtbl.t;
  symbols_inv: (string,lit) Hashtbl.t;

  mutable comments:string list;
}

val copy : t -> t
  
(** And gates of the AIG in increasing left hand side *)
val gates : t -> (lit*lit*lit) list

val read : in_channel -> t
val read_from_file : string -> t
val write : out_channel -> t -> unit
val write_to_file : t -> string -> unit

val empty : unit -> t

exception AlreadyExists

val add_input : t -> string -> lit
val add_latch : t -> string -> lit
val set_latch_update : t -> lit -> lit -> unit
val set_output : t -> string -> lit -> unit

(** [conj aiger rhs0 rhs1] gives [lhs]*)
val conj : t -> lit -> lit -> lit
val disj : t -> lit -> lit -> lit

val add_comment : t -> string -> unit

(** String corresponding to literals *)
val lit2string : t -> lit -> string option
val string2lit : t -> string -> lit option


exception Correspondance_not_found of string
(** These functions may raise [Correspondance_not_found] *)
val lit2string_exn : t -> lit -> string
val string2lit_exn : t -> string -> lit

(** More practical way to access litterals *)
type tag = Constant of bool | Input of lit | Latch of (lit*lit) | And of (lit*lit*lit) | Output of lit

val lit2tag : t -> lit -> tag option
(** [lit2tag_exn] may raise [Not_found] exception *)
val lit2tag_exn : t -> lit -> tag

(** remove an output, may raise an exception [Not_found] if the name does not correspond to any literal, and [Not_output] if it is not an output *)
exception Not_output of tag
val hide : t -> string -> unit

(** List of names used as symbols. *)
val names : t -> string list
val inputs : t -> string list
val outputs : t -> string list
val latches : t -> string list

(** Rename variables of the aiger file according to the given correspondance *)
val rename : t -> (string -> string) -> unit

(** Merge 2 aiger files where symbols with the same name are merged. *)
val compose : t -> t -> t

