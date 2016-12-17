# Reglisse
Generates hardware description from safety conditions given by regular languages.

## Installation
You need ocaml-cudd and Speculoos to compile this program.
Installation scripts are provided within this package.
Execute the following commands:

    ./install_ocaml_cudd.sh
    make

This should produce an executable called `reglisse`.
You can test it using one of the examples, for instance `./reglisse examples/ex1.rgl`.

## Usage
To launch the program, call it with the name of a file: `./reglisse examples/ex1.rgl`.
The file should contain a list of input and output and safety specifications described by regular expressions.
If the specification are realizable, the program creates a verilog file describing a module that is correct with respect with the safety specifications.


## Reglisse syntax

We distinguish three types of modules: _functional_ modules, _procedural_ modules and _compositional_ modules. 

All types of module start with a statement of the form:

    module Mname (input i1, input i2, output o1);

and end with

    endmodule


## Functional modules

These modules follow the minispec syntax of ocaml-aiger.
Here are examples of such modules:

    module Disj(input a, input b, output c);
        c <- a | b;
    endmodule

    module Conj(input a, input b, output c);
       c <- a & b;
    endmodule

    module Register(input a, output r);
       reg b1;
       r <- b1;
       b1 <- a;
    endmodule

## Procedural modules

These are lists of conditions of the form:

    if "reg_exp" then "sequence";
    never "reg_exp";
    ....

We plan to add more instructions, the goal would be to be able to encode any cosafe or safe LTL formula.

## Composition modules

Composition module are lists of module calls, like for instance, this example from functional.rgl:

    module Main(input a, input b, output c, output d, output r);
      Disj(a,b,c);
      Conj(a,b,d);
      Register(a,r);
    endmodule 
