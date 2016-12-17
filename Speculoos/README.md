# Speculoos

Speculoos (Specification Logics for Synthesis) is a set of tools for Register Transfer Level hardware description.
It has a rich type system and generates AIGER files.

## Installation

To install the tool, ocaml-cudd and ocaml-aiger need to be installed in the parent directory of Speculoos.
To install ocaml-aiger, go to the parent directory of Speculoos then enter the following commands:
> git clone https://github.com/romainbrenguier/ocaml-aiger.git
> && cd ocaml-aiger 
> && make


To install ocaml-cudd, go to the parent directory of Speculoos then enter the following commands:
> git clone https://github.com/romainbrenguier/ocaml-cudd.git
> && cd ocaml-cudd
> && ./install_cudd.sh
> && make

When this is done run:
> make

This compiles the Speculoos Compiler and the necessary libraries.
To test the compiler on some examples, run:

> make test

## The Speculoos language
The Speculoos compilers accepts some basic language that can be compiled to AIGER.
Some examples are provided in the examples directory.
However Speculoos is more powerful when used through its Libraries

To compile one example, try:
> ./speculoosCompiler.byte examples/ex1.spec

### Variables 
Variables are declared in the begining of the file.
The syntax is:
> var <variable_name> : <variable_type>;

The type can be unsigned integer: int <width>, Boolean: bool, arrays of other type: (<cell_type>)[<size>], record: { <field_name>: <type> ; ... } or union type: <Constructor_name> of <type> | ...
Here is an example that illustrates all this declarations:

> var i : int 4;

> var b : bool;

> var arr : (int 8)[4];

> var state : { x : int 4; y : int 4; time : int 4 };

> var position : Pos of int 7 | Neg of int 7 ;

Optionaly variables can be given an initial value using the syntax:
> init <variable_name> <- <initial_value> ;

If no initial value is given, every bit is considered to be 0

Variable declarations are followed by the description of variables' updates which are given using the syntax: <variable_name> <- <expression>;

### Constants

It is possible to use integer constants and the Boolean true and false constants.


### Conditional instructions

There are to two kinds of conditional instructions "when" and "if".

> when (gate[0]) { gate[1] <- ! gate[2]; } 

> if (gate[1]) then {gate[0] <- gate[2]; } else {gate[2] <- gate[0];}

If several instructions update the same variable, the last one for which the conditions holds is applied.

### Expressions

Speculoos provides the following Boolean operators, given by order of precedence:

! : negation
 
>> and << : right shift and left shift by an constant number of bits

 & : bitwise conjunction

|| : bitwise disjunction

^ : bitwise xor

<-> : bitwise equivalence
 
--> : bitwise implication

? : : if then else expression


It also provides the following operations on unsigned integers:

mod : remainder in division

/ : division

* : multiplication

- : substraction

+ : addition

=, <=, <, >, >= : comparison



### Accessing fields of complex datatypes

Fields of records are accessed using the 'dot' operator:
> when (state.y < 10 & input) {state.y <- state.y + 1;} 

Elements of an array are accessed using the [] operator: 
> arr[2] <- arr[1] + arr[0];

Union types are accessed through pattern matching:
> position <- match position with
    | Pos i -> (i > 0) ? Pos (i - 1) : Neg 1
    | Neg i -> Neg (i + 1)



## Ocaml Library
### Compiling with Speculoos

We recommend using ocamlbuild. For instance the command:
> ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/rising_edge.native --

builds and executes the program in examples/rising_edge.ml.
You can basically substitute in this command any ml file that you write.

### Datatypes

For now only Booleans and unsigned integers are supported.
Constant values can be declared in that way:
> let b = bool true

> let i = int 5

The keyword "let" is part of Ocaml and is used for variables.
"true" and "5" are Ocaml Booleans and integers.

Speculoos variables are declared in that way:
> let x = var "x" (Type.int 8)

The string argument is the name the variable will be refered to in the synthesized circuit (which could be different from the name of the Ocaml variable).
The argument after Type.int is the width (number of bits) for this value (8 in the example).
Speculoos will automatically infer whether x should be an inpout, output or register.


### Expressions

To create circuit we use textual expressions:
> let d = (a $& b) $| (neg c $& d)

where $&, $|, neg represent bitwise AND, OR and NOT respectively.
The symbol $ is used to distinguish these from there Ocaml equivalent.
The name d represent a wire whose width is infered from the expression on the right.

It is possible to select a subset of bits from an expression using the function select:
> let e = select d [5,0]

This selects the first 6 bits of d and put them in reverse order in e.

### Outputs and updates

A circuit is generated from the description of register updates and outputs expressions.
For instance:
> let x = var "x" Type.bool 

> let previous = var "previous" Type.bool 

> let _ = compile [var "rising_edge", (x $& neg previous); previous, x]

generates a circuit in which x is an input, previous is a register that record the value of x at the previous clock cycle, and rising_edge is an output true when the current value is true and the previous is false.
Please see the file in examples/rising_edge.ml for the full program.
