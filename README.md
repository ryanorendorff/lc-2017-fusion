Fusion: Applying Equational Transforms to Simplify Programs
===========================================================


Introduction
------------

Fusion is a technique of transforming programs to eliminate unnecessary computations that do not alter the result of the program. This technique is used heavily in compilers for functional programming languages, the most prevalent example being the shortcut fusion algorithm used by the Glasgow Haskell Compiler. This session will delve into how the Haskell compiler performs fusion using rewrite rules, which allow a programmer to state that two lines of code are equivalent.


Pitch
-----

Fusion allows a programmer to write modular programs that are as performant as hand-tuned code.


Concepts
--------

The developer will learn about how to automatically create optimized forms of their programs through the following topics:
- What fusion and rewrite rules are, and how these techniques are used in functional programming language compilers.
- A basic overview of some fusion systems used in practice, such as stream fusion.
