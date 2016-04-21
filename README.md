STG interpreter
===============

The spineless tagless graph reduction machine, STG for short, is an automaton
used to map non-strict functional languages onto stock hardware. It was
developed for, and is heavily used in, [the Haskell compiler GHC][ghc].

This project implements an interpreter for the STG as it is [described in the
1992 paper on the subject][stg1992], considering readability and staying close
to the source above all other considerations.

Since 1992, the STG has undergone one notable revision, documented in [the 2004
paper *How to make a fast curry*][fastcurry]. The plan is to migrate to this
more modern implementation, which treats function application a bit different,
once I'm satisfied with the code, and still curious about the STG.

[ghc]: https://www.haskell.org/ghc/
[stg1992]: http://research.microsoft.com/apps/pubs/default.aspx?id=67083
[fastcurry]: http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/



To do
=====

- Main functionality
    - Test the stepper
    - Write lots of test cases
    - Unify primitive and algebraic alts into a single type a la
      `Alts [Alt] Default`, `Alt = Algebraic | Primitive`
    - Remove all `error`s
    - Unify the stacks to one; should be more instructional. For example, an
      update frame is very clearly flattened into the argument and return
      frames it contains when an update is triggered. Should also reduce the
      nesting of stacks in the overview.
- Nice to have
    - Explain steps better
    - Command line options (in particular to control colouring)
    - Convenience functions for Prelude, for example to generate a list of
      numbers
    - Unify Pretty and PrettyAnsi instances by calling parametrized generic
      prettyprinters. A member definition would then look like
      `pretty = prettyLambda pretty pretty`
      `prettyAnsi = prettyLambda prettyAnsi prettyAnsi`
    - Separate lexer and parser
    - Annotate syntax tree
    - Highlight error location in input
    - HTML prettyprinter
- Small stuff
