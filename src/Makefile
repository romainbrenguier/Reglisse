

all: reglisse.byte

clean:
	ocamlbuild -clean
	rm -f *.ppo *.ppr *.ast
	rm ocaml-cudd.tar.gz
	rm ocaml-aiger.tar.gz

doc:
	ocamlbuild speculog.docdir/index.html

reglisse.byte:
	ocamlbuild reglisse.byte

test: reglisse.byte
	./reglisse.byte examples/washing.rgl

