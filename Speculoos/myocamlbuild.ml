open Ocamlbuild_plugin
open Command


let () =
  dispatch 
    (
      function
      | After_rules ->
	flag ["doc";"use_ocaml-aiger"] (S[A "-I"; P "../../aiger/_build"]);
	flag ["ocaml";"compile";"use_ocaml-aiger"] (S[A "-I"; P "../../aiger/_build"]);
	flag ["ocaml";"link";"use_ocaml-aiger";"byte"] (S[P "../../aiger/_build/aigerImperative.cma"]);
	flag ["ocaml";"link";"use_ocaml-aiger";"native"] (S[P "str.cmxa"; P "../../aiger/_build/aigerImperative.cmx"]);
	flag ["ocaml";"compile";"use_ocaml-cudd"] (S[A "-I"; P "../../ocaml-cudd/_build/"]);
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
	flag ["ocaml";"link";"use_ocaml-cudd";"byte"] 
	  (S [
	    A"-custom";
	    P "../../ocaml-cudd/_build/cudd.o";
	    P"../../ocaml-cudd/_build/cudd.cmo"; 
	    P"../../ocaml-cudd/cudd-2.5.0/cudd/libcudd.a";
	    P"../../ocaml-cudd/cudd-2.5.0/util/libutil.a";
	    P"../../ocaml-cudd/cudd-2.5.0/epd/libepd.a";
	    P"../../ocaml-cudd/cudd-2.5.0/mtr/libmtr.a";
	    P"../../ocaml-cudd/cudd-2.5.0/st/libst.a";
	  ]);
	flag ["ocaml";"link";"use_ocaml-cudd";"native"] 
	  (S [
	    P "../../ocaml-cudd/_build/cudd.o";
	    P"../../ocaml-cudd/_build/cudd.cmxa"; 
	    P"../../ocaml-cudd/cudd-2.5.0/cudd/libcudd.a";
	    P"../../ocaml-cudd/cudd-2.5.0/util/libutil.a";
	    P"../../ocaml-cudd/cudd-2.5.0/epd/libepd.a";
	    P"../../ocaml-cudd/cudd-2.5.0/mtr/libmtr.a";
	    P"../../ocaml-cudd/cudd-2.5.0/st/libst.a";
	  ]);
      | _ -> ()
    )
