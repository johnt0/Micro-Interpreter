# MicroCaml Interpreter
The MicroCaml interpreter is a program that can evaluate expressions and statements written in a subset of the OCaml language. The interpreter is written in OCaml and is designed to be used as a command-line tool.

## Installation
To install the MicroCaml interpreter, you must have OCaml and its package manager, OPAM, installed on your system. Once you have these tools, you can install the interpreter by running the following command:

```opam install microcaml```

This command will install the MicroCaml interpreter and all its dependencies on your system.

## Usage
To use the MicroCaml interpreter, simply run the command:

```dune exec bin/mutop.exe```

This will run the code in myfile.ml and print any output to the console.

## Syntax
The MicroCaml interpreter supports a subset of the OCaml language, including:

- Integers: 1, 2, 3, etc.
- Booleans: true, false
- Strings: "hello", "world", etc.
- If statements: if x > 0 then "positive" else "negative"
- Let bindings: let x = 1 + 2 in x * 3
- Functions: fun x -> x + 1
- Function application: (fun x -> x + 1) 2
- The full syntax of the language is defined in the MicroCamlTypes.ml file in the interpreter source code.

## Limitations
The MicroCaml interpreter is not a complete implementation of the OCaml language, and there are several features that are not supported. These include:

Modules
Pattern matching
Recursive functions (although recursive let bindings are supported)