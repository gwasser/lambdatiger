Overview Of Compiler Internals
==============================

A compiler is a large piece of software. Compilers ultimately translate source
code, text written in a human-readable programming language, into binary machine
code that can be executed by a computer. The human-readable programming language
is written in text, but consists of features like types, functions, etc., that
help the programmer to structure ideas into ways that make sense for translation
into binary code. Those abstract features don't exist at the machine level
though, so have to be analyzed and translated.

This whole long process is therefore split into several phases. Appel describes
the phases as follows:

- **Lex** --> break the source file into individual words, or *tokens*.
- **Parse** --> analyze the phrase structure of the program to form a
  *concrete syntax tree* representing the source token structure.
- **Semantic Actions** --> build a piece of the *abstract syntax tree*
  corresponding to each phase.
- **Semantic Analysis** --> determine what each phase means, relate uses of
  variables to their definitions, check types of expressions, request
  translation of each phrase.
- **Frame Layout** --> place variables, function parameters, etc., into
  *activation records* (or, *stack frames*) in a machine-dependent way.
- **Translate** --> produce *intermediate representation trees* (IR trees), a
  notation that is not tied to any particular source language or target machine
  architecture.
- **Canonicalize** --> hoist side effects out of expressions, and clean up
  conditional branches, for the convenience of the next phases.
- **Instruction Selection** --> group the IR tree nodes into clumps that
  correspond to the actions of target machine instructions.
- **Control Flow Analysis** --> analyze the sequence of instructions into a
  *control flow graph* (CFG) that shows all the possible flows of control the
  program might follow when it executes.
- **Dataflow Analysis** --> gather information about the flow of information
  through variables of the program, for example, *liveness analysis*
  calculates the places where each program variable holds a still-needed
  value (is *live*).
- **Register Allocation** --> choose a register to hold each of the variables and
  temporary values used by the program; variables not live at the same time
  can share the same register.
- **Code Emission** --> replace the temporary names in each machine instruction
  with machine registers.

Often these phases are collected into three main categories, which
[Wikipedia][1] describes as:

- *Front-End* --> The front end scans the input and verifies syntax and
  semantics according to a specific source language, including lexical,
  syntax, and semantic analysis. The front end usually emits an
  intermediate representation (IR).
- *Middle-End* --> The middle end performs optimizations on the IR that are
  independent of the machine architecture being targeted, such as dead-code
  analysis, reachability analysis, etc.
  The middle end usually emits an "optimized" IR.
- *Back-End* --> The back end performs synthesis from the optimized IR from
  the middle end. It may perform more analysis, transformations and
  optimizations that are specific for the target machine architecture.
  The back end generates the target-dependent assembly code.

This tutorial will adopt that convention, and follow the phases as described
by Appel and roughly split parts of the tutorial into the three high-level
phases of a Front-End, Middle-End, and Back-End.

[1]: https://en.wikipedia.org/wiki/Compiler#Three-stage_compiler_structure

