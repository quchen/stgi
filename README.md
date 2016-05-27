STG interpreter [![](https://travis-ci.org/quchen/stg.svg?branch=master)](https://travis-ci.org/quchen/stg)
===============

A user-centric visual STG implementation to help understand Haskell's execution
model.

![](screenshot.png)


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

- Function application uses no parentheses or commas like in Haskell `f x y z`,
  not with curly parentheses and commas like in the paper `f {x,y,z}`.
- Comment syntax like in Haskell
- Constructors can end with a `#` to allow labelling primitive boxes
  e.g. with `Int#`.
- A lambda's head is written `\(free) bound -> body`, where `free` and
  `bound` are space-separated variable lists, instead of the paper's
  `{free} \n {bound} -> body`, which uses comma-separated lists. The
  update flag `\u` is signified using a double arrow `=>` instead of the
  normal arrow `->`.

### Evaluation

- The three stacks from the operational semantics given in the paper - argument,
  return, and update - are unified into a single one, since they run
  synchronously anyway. This makes the current location in the evaluation much
  clearer, since the stack is always popped from the top. For example, having a
  return frame at the top means the program is close to a `case` expression.
- There are different objects on the heap, not just closures:
    - Closures are all represented alike, but classified for the user in the
      visual output:
        - Constructors are closures with a constructor application body, and
          only free variables.
        - Other closures with only free variables are thunks.
        - Closures with non-empty argument lists are functions.
    - Black holes overwrite updatable closures upon entering, allowing for
      `<<loop>>` detection and avoiding certain space leaks (... apparently,
      at least the 1992 paper says so).


GHC's current STG
-----------------

Since 1992, the STG has undergone one notable revision, documented in [the 2004
paper *How to make a fast curry*][fastcurry], which treats function application
a bit different. I don't have plans to support this evaluation model right now,
but it's on my list of long-term goals (alongside the current push/enter).


Short language introduction
---------------------------

The STG language can be seen as a mostly simplified version of Haskell with a
couple of lower level additions. Most importantly, STG is a wholly untyped
language.


### Top-level

An STG program consists of a set of bindings, which each have the form

```haskell
name = \(<free vars>) <bound vars> -> <expression body>
```

The right-hand side is called a *lambda form*, and is closely related to the
usual lambda from Haskell.

  - Bound variables are the lambda paramaters just like in Haskell.
  - Free variables are the variables used in the `body` that are not bound or
    global. This means that variables from the parent scope are not
    automatically in scope, but you can get them into scope by adding them to
    the free variables list.

### Expressions

Expressions can, in general, be one of a couple of alternatives.

  - **Letrec**

    ```haskell
    letrec <...bindings...> in <expression>
    ```

    Introduce local definitions, just like Haskell's `let`.

  - **Let**

    ```haskell
    let <...bindings...> in <expression>
    ```

    Like `letrec`, but the bindings cannot refer to each other (or themselves).
    In other words, `let` is non-recursive.

  - **Case**

    ```haskell
    case <expression> of <alts>
    ```

    Evaluate the `<expression>` (called scrutinee) to WHNF and continue
    evaluating the matching alternative. Note that the WHNF part makes case
    strict, and indeed it is the *only* construct that does evaluation.

    The `<alts>` are semicolon-separated list of alternatives of the form

    ```haskell
    Constructor <args> -> <expression> -- algebraic
    1# -> <expression>                 -- primitive
    ```

    and can be either all algebraic or all primitive. Each list of alts must
    include a default alternative at the end, which can optinally bind a
    variable.

    ```haskell
    v -> <expression; v is in scope> -- bound default
    default -> <expression>          -- unbound default
    ```

  - **Function application**

    ```haskell
    function <args>
    ```

    Like Haskell's function application. The `<args>` are primitive values or
    variables.

  - **Primitive application**

    ```haskell
    primop# <arg1> <arg2>
    ```

    Primitive operation on unboxed integers.

    The following operations are supported:

      - Arithmetic
        - `+#`: addition
        - `-#`: subtraction
        - `*#`: multiplication
        - `/#`: integer division (truncated towards -∞)
        - `%#`: modulo (truncated towards -∞)
      - Boolean, returning `1#` for truth and `0#` for falsehood:
        `<#`, `<=#`, `==#`, `/=#`, `>=#`, `>#`

  - **Constructor application**

    ```haskell
    Constructor <args>
    ```

    An algebraic data constructor applied to a number of arguments, just like
    function application. Note that constructors always have to be saturated
    (not partially applied); to get a partially applied constructor, wrap it in
    a lambda form that fills in the missing arguments with parameters.

  - **Primitive literal**

    An integer postfixed with `#`, like `123#`.

For example, Haskell's `maybe` function could be implemented in STG like this:

```haskell
maybe = \just nothing x -> case x of
    Just j   -> just j;
    Nothing  -> nothing
    badMaybe -> Error_badMaybe badMaybe
```

Some lambda expressions can only contain certain sub-elements; these special
cases are detailed in the sections below. To foreshadow these issues:

- Lambda forms always have lifted (not primitive) type
- Lambda forms with non-empty argument lists and standard constructors are never
  updatable


### Updates

A lambda form can optionally use a double arrow `=>`, instead of a normal arrow `->`.
This tells the machine to update the lambda form's value in memory once it has
been calculated, so the computation does not have to be repeated should the
value be required again. This is the mechanism that is key to the lazy
evaluation model the STG implements. For example, evaluating `main` in

```haskell
add = <add two boxed ints>
one = \ -> Int# 1#;
two = \ -> Int# 2#;
main = \ => add2 one two
```

would, once the computation returns, overwrite `main` (modulo technical
details) with

```haskell
main = \ -> Int# 3#
```

A couple of things to keep in mind:

- Closures with non-empty argument lists and constructors are already in WHNF,
  so they are never updatable.
- When a value is only entered once, updating it is unnessecary work. Deciding
  whether a potentially updatable closure should actually be updatable is what
  the *update analysis* would do in a compiler when translating into the STG.



### Pitfalls

- Semicolons are an annoyance that allows the grammar to be simpler. This
  tradeoff was chosen to keep the project's code easier to read, as it does
  not require indentation handling in the grammar.

- Lambda forms stand for deferred computations, and as such cannot have
  primitive type, which are always in normal form. To handle primitive types,
  you'll have to box them like in

  ```haskell
  three = \ -> Int# 3#
  ```

  Writing

  ```haskell
  three' = \ -> 3#
  ```

  is invalid, and the machine would halt in an error state.

- Function application cannot be nested, since function arguments are primitives
  or variables. Haskell's `map f (map g xs)` would be written

  ```haskell
  let map_g_xs = \ -> map g xs
  in map f map_g_xs
  ```

  assuming all variables are in global scope. This means that nesting functions
  in Haskell results in a heap allocation via `let`.

- Free variable values have to be explicitly given to the closure. Function
  composition could be implemented like

  ```haskell
  compose = \f g x -> let gx = \(g x) -> g x
                      in f gx
  ```

  Forgetting to hand `g` and `x` to the `gx` lambda form would mean that in the
  `g x` call neither of them was in scope, and the machine would halt with
  a "variable not in scope" error.


### Code example

The 1992 paper gives two implementations of the `map` function in section 4.1.
The first one is the STG version of

```haskell
map f [] = []
map f (y:ys) = f y : map f ys
```

which, in this STG implementation, would be written

```haskell
map = \f xs -> case xs of
    Nil -> Nil;
    Cons y ys -> let fy = \(f y) => f y;
                     mfy = \(f ys) => map f ys
                 in Cons fy mfy;
    badList -> Error_map badList
```

For comparison, the paper's version is

```haskell
map = {} \n {f,xs} -> case xs of
    Nil {} -> Nil {}
    Cons {y,ys} -> let fy = {f,y} \u {} -> f {y}
                       mfy = {f,ys} \u {} -> map {f,ys}
                   in Cons {fy,mfy}
    badList -> Error_map {badList}
    -- (The paper omits the default case for readability)
```

You can find lots of further examples of standard Haskell functions implemented
by hand in STG in the `Prelude` modules. Combined with the above explanations,
this is all you should need to get started.


Running an STG program
----------------------

The `app/Main.hs` file is written so you can easily switch out the `prog` value.
The `Stg.ExamplePrograms` module provides a number of programs that might be
worth having a look, and are a good starting point for modifications or adding
your own programs.

When you're happy with your `app/Main.hs`, run

```bash
stack build --exec "stg-exe --colour=true" | less -R
```

to get coloured output in `less`. Type `/====` to search for `====`, which
finds the top of every new step; use `n` (next step) or `N` (previous step) to
navigate through the execution.



### Special conditions

#### Unhelpful error?

The goal of this project is being useful to human readers. If you find an error
message that is unhelpful or even misleading, please report it as a bug!

#### Black holes

The heap does not only contain closures (lambda forms with values for the free
variables), but also black holes. Black holes are annotated with the step in
which they were created; this annotation is purely for display purposes, and not
used by the machine.

At runtime, when an updatable closure is entered (evaluated), it is overwritten
by a black hole. Black holes do not only provide better overview over what
thunk is currently evaluated, but have two useful technical benefits:

1. Memory mentioned only in the closure is now ready to be collected,
   avoiding certain space leaks. [The 1992 paper][stg1992] gives the following
   example in section 9.3.3:

   ```haskell
   list = \(x) => <long list>
   l = \(list) => last list
   ```

   When entering `l` without black holes, the entire `list` is kept in memory
   until `last` is done. On the other hand, overwriting `l` with a black hole
   upon entering deletes the `last` pointer from it, and `last` can run, and be
   garbage collected, incrementally.

2. Entering a black hole means a thunk depends on itself, allowing the
 interpreter to catch some non-terminating computations with a useful error





[ghc]: https://www.haskell.org/ghc/
[stg1992]: http://research.microsoft.com/apps/pubs/default.aspx?id=67083
[fastcurry]: http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/
