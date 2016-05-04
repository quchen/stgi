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

- The three stacks from the paper - argument, return, and update - are unified
  into a single one, since they run synchronously anyway. This makes the current
  location in the evaluation much clearer, since the stack is always popped from
  the top. For example, having a return frame at the top means the program is
  close to a `case` expression.


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
