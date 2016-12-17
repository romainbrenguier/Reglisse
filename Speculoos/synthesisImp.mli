(** The first component gives the expressions that are non synthesizable and raised the exception, the second one gives an expression describing the non synthesizable inputs. *)
exception NonSynthesizable of (Boolean.t * Boolean.t)


(** Synthesize from the definition of how each variable should be updated.
    The expression on the left should be an output or a register.
    Warning: if the expression on the right has size greater than the variable on the left, then its result is truncated. 
*)
val functional_synthesis : (Integer.t * Integer.t) list -> AigerImperative.t

