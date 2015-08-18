

all: main.byte simulation.byte

clean:
	ocamlbuild -clean
	rm -f *.ppo *.ppr *.ast
	rm -f ocaml-cudd.tar.gz
	rm -f ocaml-aiger.tar.gz

doc:
	ocamlbuild speculog.docdir/index.html

main.byte: *.ml
	ocamlbuild main.byte

simulation.byte: *.ml
	ocamlbuild simulation.byte

test: main.byte
	./main.byte examples/washing.rgl

