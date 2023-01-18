# wabi interpreter

** WARNING**

WORK IN PROGRESS DO NOT USE

# build

1. install zig version 0.11.0

2. build the interpreter
   > zig build -Doptimize=ReleaseFast

3. build the repl image
   > zig build genesis < repl.wabi

4. have (repl)fun
   > zig-out/bin/wabivm repl.fasl

## some principles

1. control is first class through delimited continuations (``prompt`/`control` operators)

2. syntax is first class via fexpressions (see kernel programming language), that implies environments are first class

3. "special" operators are not special (like `if` `fn` `fx`): They are just builtins.

4. All the resources are constrained (i.e. memory, cpu ec...)

## State

whatever can be calculuated in the initial allocated memory is ok, there is a bug in the algorithm
that copies active memory in another memory space (i.e. the equivalent of GC for other languages)
that prevents the system to continue.

### Missing pieces

* fix bugs :(

* use a smarter data structure for lists

* introduce maps (HAMT)

* introduce sets (or not? the same of a map to boolean)

* extend the numeric tower with arbirtrarly big integers, rationals, imaginary (inexact number?)

* define a (first classs) module system

* IO using callbacks/continuations

### examples

see the tests directory
