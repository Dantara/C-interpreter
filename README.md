# C Interpreter

This repository contains interpreter and pretty printer for a small subset of C language.

This application build on Haskell programming language and relies on combination 
of [Alex](https://hackage.haskell.org/package/alex) and [Happy](https://hackage.haskell.org/package/happy) 
to generate code for a lexer and a parser. 

*NOTE*: This application was created for education purpose.

### Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended to get Stack with batteries included by
installing [Haskell Platform](https://www.haskell.org/platform/).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC.

## Run

This project relies on [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
to parse command line arguments. 
**Stack** has poor support of command line arguments, therefore it is recommended
to run the following command to copy binaries into your *PATH*:

```sh
stack install
```

After it, to run interpreter you should run the following command:

``` sh
c-interpreter-exe FILE_NAME
```

Where *FILE_NAME* is a path for a file with the source code.

To run *pretty-printer* you should execute the following command:

``` sh
c-interpreter-exe SOURCE_FILE -p TARGET_FILE
```

## Examples of C programs

All examples of programs that can be interpreted via this interpretator 
are placed into *code_samples* folder.
