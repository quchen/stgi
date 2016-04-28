STG interpreter
===============

About the machine
-----------------

The spineless tagless graph reduction machine, STG for short, is an automaton
used to map non-strict functional languages onto stock hardware. It was
developed for, and is heavily used in, [the Haskell compiler GHC][ghc].

This project implements an interpreter for the STG as it is [described in the
1992 paper on the subject][stg1992], considering readable code and staying close
to the source paper above all other considerations.


Useful applications
-------------------

These are some of the questions the STG answers:

1. Does this leak memory? If yes, then on the heap or on the stack, and why?
2. I heard GHC doesn't have a call stack. How does that work?


GHC's current STG
-----------------

Since 1992, the STG has undergone one notable revision, documented in [the 2004
paper *How to make a fast curry*][fastcurry], which treats function application
a bit different. Due to the large change necessary to adapt to this change, and
the small amount of knowledge to be gained compared to having an actual working
STG, I currently have no plans to migrate. The most important questions are
answered by both versions of the machine alike; it merely turned out that one of
them is more efficient as an actual high-performance Haskell compiler backend
language.

[ghc]: https://www.haskell.org/ghc/
[stg1992]: http://research.microsoft.com/apps/pubs/default.aspx?id=67083
[fastcurry]: http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/



To do
=====

- Main functionality
    - Rule 18/19 on page 65: Better primitive case
    - Test the stepper
    - Write lots of test cases
    - Unify the stacks to one; should be more instructional. For example, an
      update frame is very clearly flattened into the argument and return
      frames it contains when an update is triggered. Should also reduce the
      nesting of stacks in the overview.
- Nice to have
    - Refactor the fugly let rule
    - Command line options (in particular to control colouring)
    - Unify Pretty and PrettyAnsi instances by calling parametrized generic
      prettyprinters. A member definition would then look like
      `pretty = prettyLambda pretty pretty`
      `prettyAnsi = prettyLambda prettyAnsi prettyAnsi`
    - Separate lexer and parser
    - Annotate syntax tree
    - Highlight error location in input
    - HTML prettyprinter
- Small stuff
