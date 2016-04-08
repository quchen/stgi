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

- Small stuff
    - [ ] Move OverloadedStrings into other-extensions, don't enable it by
          default
    - [ ] No `String` in types, use `Text` only
