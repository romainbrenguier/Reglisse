open Ocamlbuild_plugin
open Command


let () =
  dispatch 
    (
      function
      | After_rules ->
	flag ["doc";"use_ocaml-aiger"] (S[A "-I"; P "../../ocaml-aiger/_build"]);
	flag ["ocaml";"compile";"use_ocaml-aiger"] (S[A "-I"; P "../../ocaml-aiger/_build"]);
	flag ["ocaml";"link";"use_ocaml-aiger"] (P "../../ocaml-aiger/_build/aiger.cma");
	flag ["ocaml";"compile";"use_ocaml-cudd"] (S[A "-I"; P "../../ocaml-cudd/"]);
	flag ["ocaml";"compile";"syntax_extension"] 
	     (S[
		  A "-I"; P"+camlp4";A"-pp";
		  Quote (S[P"camlp4o";P"pa_extend.cmo";P"q_MLast.cmo"]);
		  A "-dtypes"
	     ]);
	flag ["ocaml";"compile";"my_syntax"] 
	     (S[
		  A "-dtypes";
		  A"-pp";
		  Quote (S[P"_build/pa_speculog.cmo";P"camlp4o";P"pa_extend.cmo";P"q_MLast.cmo"]);
	     ]);
	flag ["ocaml";"compile";"camlp4"] (S[A"-I";P"camlp4-lib-dir";A"-I";P"+camlp4"]);
	flag ["ocaml";"link";"use_ocaml-cudd"] 
	  (S [
	    A"-custom";
	    P "../../ocaml-cudd/cudd.o";
	    P"../../ocaml-cudd/cudd.cmo"; 
	    P"../../ocaml-cudd/cudd-2.5.0/cudd/libcudd.a";
	    P"../../ocaml-cudd/cudd-2.5.0/util/libutil.a";
	    P"../../ocaml-cudd/cudd-2.5.0/epd/libepd.a";
	    P"../../ocaml-cudd/cudd-2.5.0/mtr/libmtr.a";
	    P"../../ocaml-cudd/cudd-2.5.0/st/libst.a";
	  ]);
      | _ -> ()
    )
