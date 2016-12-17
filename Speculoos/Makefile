

all: speculoosCompiler.byte circuit.cmo

pa_speculog.cmo: 
	ocamlbuild expression.cmx
	ocamlbuild speculog.cmx
	ocamlbuild aigerBdd.cmx
	camlp4o pa_extend.cmo q_MLast.cmo pr_o.cmo pa_speculog.ml -o pa_speculog.ppo 
	camlp4o pa_extend.cmo q_MLast.cmo pa_speculog.ml -o pa_speculog.ast
	ocamlbuild pa_speculog.cmo


cycles: _build/examples/scheduling.byte

simulation.byte: *.ml
	ocamlbuild simulation.byte

circuit.cmo: *.ml
	ocamlbuild circuit.cmo

speculoosCompiler.byte: *.ml
	ocamlbuild speculoosCompiler.byte

regularExpression.byte: *.ml
	ocamlbuild regularExpression.byte

acacia2aig.native: *.ml examples/acacia2aig.ml
	ocamlbuild -tag use_camlp4 -tag camlp4o -tag use_ocaml-cudd -tag use_ocaml-aiger examples/acacia2aig.native

acacia2aig.byte: *.ml examples/acacia2aig.ml
	ocamlbuild -tag use_camlp4 -tag camlp4o -tag use_ocaml-cudd -tag use_ocaml-aiger examples/acacia2aig.byte

addinputs2aig.native: *.ml examples/addinputs2aig.ml
	ocamlbuild -tag use_camlp4 -tag camlp4o -tag use_ocaml-cudd -tag use_ocaml-aiger examples/addinputs2aig.native

addinputs2aig.byte: *.ml examples/addinputs2aig.ml
	ocamlbuild -tag use_camlp4 -tag camlp4o -tag use_ocaml-cudd -tag use_ocaml-aiger examples/addinputs2aig.byte

clean:
	ocamlbuild -clean
	rm -f *.ppo *.ppr *.ast

doc:
	ocamlbuild -tag use_ocaml-aiger speculoos.docdir/index.html

test: speculoosCompiler.byte
	./speculoosCompiler.byte examples/ex1.spec -o ex1.aig
	./speculoosCompiler.byte examples/ex2.spec -o ex1.aig
	./speculoosCompiler.byte examples/ex3.spec -o ex2.aig
	./speculoosCompiler.byte examples/record.spec -o record.aig
	./speculoosCompiler.byte examples/array.spec -o array.aig
	./speculoosCompiler.byte examples/union.spec -o union.aig
	./speculoosCompiler.byte examples/conditionals.spec -o conditionals.aig
	./speculoosCompiler.byte examples/rising_edge.spec -o rising_edge.aig

matrix: 
	mkdir matrix
	ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/matrix.byte --


_build/examples/scheduling.byte:
	mkdir cycles; ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/scheduling.byte --
