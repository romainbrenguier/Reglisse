

all: reglisse.byte

clean:
	ocamlbuild -clean
	rm -f *.ppo *.ppr *.ast
	rm -f ocaml-cudd.tar.gz
	rm -f ocaml-aiger.tar.gz

doc:
	ocamlbuild speculog.docdir/index.html

reglisse.byte: *.ml
	ocamlbuild reglisse.byte

test: reglisse.byte
	./reglisse.byte examples/washing.rgl

