# Reglisse
Generates hardware description from safety conditions given by regular languages.

## Installation
You need ocaml-cudd and ocaml-aiger to compile this program.
Installation scripts are provided within this package.
Execute the following commands:

> `./install_ocaml_aiger.sh`

> `./install_ocaml_cudd.sh`

> `make`

This should produce an executable called `reglisse`.
You can test it using one of the examples, for instance `./reglisse examples/ex1.rgl`.

## Usage
To launch the program, call it with the name of a file: `./reglisse examples/ex1.rgl`.
The file should contain a list of input and output and safety specifications described by regular expressions.
If the specification are realizable, the program creates a verilog file describing a module that is correct with respect with the safety specifications.
