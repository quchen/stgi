STG interpreter
===============

[![Build Status](https://travis-ci.org/quchen/stg.svg?branch=master)](https://travis-ci.org/quchen/stg)

About the machine
-----------------

The spineless tagless graph reduction machine, STG for short, is an automaton
used to map non-strict functional languages onto stock hardware. It was
developed for, and is heavily used in, [the Haskell compiler GHC][ghc].

This project implements an interpreter for the STG as it is [described in the
1992 paper on the subject][stg1992], with the main focus on being nice to a
human user. Things that might be important for an actual compiler backend, such
as performance or static analysis, are not considered in general; only if it
helps the understanding of the STG.

The idea behind the machine is to represent the program in its abstract syntax
tree form. However, due to references to other parts of the syntax tree, a
program is a graph, not a tree. By evaluating this graph using a small set of
rules, it can be systematically reduced to a final value, which will be the
result of the program.

The STG is
  - **spineless** because the graph is not represented as a single data
    structure in memory, but as a set of small parts of the graph that reference
    each other. An important part of the evaluation mechanism is how to follow
    these references.
  - **tagless** because all heap values - unevaluated values, functions, already
    evaluated values - are represented alike on the heap, in form of closures.
    Tag*ful* would mean these closures have to be annotated with things type
    information, or whether they were previously evaluated already.
  - **graph reducing** because heap objects can be overwritten by simpler values
    the machine has found out to be equivalent. For example, the computation
    `1+1` on the heap might be overwritten by a constant `2` once that result
    has been obtained somewhere.


Useful applications
-------------------

These are some of the questions the STG answers:

1. Does this leak memory? If yes, then on the heap or on the stack, and why?
2. I heard GHC doesn't have a call stack. How does that work?
3. Why is this value not garbage collected?


Differences from the 1992 paper
-------------------------------

### Grammar

- Values are represented by function application to an empty argument list,
  @x ()@, as opposed to having no argument list at all in the paper.
- parentheses @()@ instead of curly braces @{}@
- Comment syntax like in Haskell
- Constructors can end with a @#@ to allow labelling primitive boxes
  e.g. with @Int#@.

### Evaluation

- The three stacks from the operational semantics given in the paper - argument,
  return, and update - are unified into a single one, since they run
  synchronously anyway. This makes the current location in the evaluation much
  clearer, since the stack is always popped from the top. For example, having a
  return frame at the top means the program is close to a `case` expression.
- There are different objects on the heap, not just closures:
    - Closures can be on the heap, as usual
    - Black holes overwrite updatable closures upon entering, allowing for
      `<<loop>>` detection and avoiding certain space leaks (... apparently,
      at least the 1992 paper says so)


GHC's current STG
-----------------

Since 1992, the STG has undergone one notable revision, documented in [the 2004
paper *How to make a fast curry*][fastcurry], which treats function application
a bit different. Due to the large change necessary to adapt to this change, and
the small amount of knowledge to be gained compared to having an actual working
STG, I *currently* have no plans to migrate. The most important questions are
answered by both versions of the machine alike; it merely turned out that one of
them is more efficient as an actual high-performance Haskell compiler backend
language.

[ghc]: https://www.haskell.org/ghc/
[stg1992]: http://research.microsoft.com/apps/pubs/default.aspx?id=67083
[fastcurry]: http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/
