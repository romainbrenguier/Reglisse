

all: reglisse.byte

clean:
	ocamlbuild -clean
	rm -f *.ppo *.ppr *.ast

doc:
	ocamlbuild speculog.docdir/index.html

reglisse.byte:
	ocamlbuild reglisse.byte

test: reglisse.byte
	./reglisse.byte examples/washing.rgl

