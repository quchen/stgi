STGi - STG interpreter
======================

STGi is a visual STG implementation to help understand Haskell's execution
model.

It does this by guiding through the runnning of a program, showing stack and
heap, and giving explanations of the applied transition rules. Here what an
intermediate state looks like:

![](screenshot.png)

[![](https://travis-ci.org/quchen/stgi.svg?branch=master)](https://travis-ci.org/quchen/stgi)

Table of contents
-----------------

- [Quickstart guide](#quickstart-guide)
- [About the machine](#about-the-machine)
- [Useful applications](#useful-applications)
- [Language introduction](#language-introduction)
	- [Top-level](#top-level)
    - [The `main` value, termination](#the-main-value-termination)
	- [Expressions](#expressions)
	- [Updates](#updates)
	- [Pitfalls](#pitfalls)
	- [Code example](#code-example)
	- [Marshalling values](#marshalling-values)
- [Runtime behaviour](#runtime-behaviour)
	- [Code segment](#code-segment)
	- [Stack](#stack)
	- [Heap](#heap)
    - [Black holes](#black-holes)
    - [Garbage collection](#garbage-collection)
	- [Unhelpful error message?](#unhelpful-error-message)
- [Differences from the 1992 paper](#differences-from-the-1992-paper)
	- [Grammar](#grammar)
	- [Evaluation](#evaluation)
- [GHC's current STG](#ghcs-current-stg)


Quickstart guide
----------------

If you want to have a quick look at the STG, here is what you need to get going.
The program should build with both [`stack`][stack] and [`cabal`][cabal].

The `app/Main.hs` file is written so you can easily switch out the `prog` value
for other `Program`s that contain a `main` definition. The `Stg.ExamplePrograms`
module provides a number of examples that might be worth having a look, and are
a good starting point for modifications or adding your own programs. It's
probably easier to read in Haddock format, so go ahead and run

```bash
stack haddock --open stgi
```

and have a look at the example programs.

When you're happy with your `app/Main.hs`, run

```bash
stack build --exec "stgi-exe --colour=true" | less -R
```

to get coloured output in `less`. Type `/====` to search for `====`, which
finds the top of every new step; use `n` (next step) or `N` (previous step) to
navigate through the execution.


About the machine
-----------------

The spineless tagless graph reduction machine, STG for short, is an automaton
used to map non-strict functional languages onto stock hardware. It was
developed for, and is heavily used in, [the Haskell compiler GHC][ghc].

This project implements an interpreter for the STG as it is [described in the
1992 paper on the subject][stg1992], with the main focus on being nice to a
human user. Things that might be important for an actual compiler backend, such
as performance or static analysis, are not considered in general, only if it
helps the understanding of the STG.

The idea behind the machine is to represent the program in its abstract syntax
tree form. However, due to references to other parts of the syntax tree, a
program is a graph, not a tree. By evaluating this graph using a small set of
rules, it can be systematically reduced to a final value, which will be the
result of the program.

The STG is
  - **spineless** because the graph is not represented as a single data
    structure in memory, but as a set of small, individual parts of the graph
    that reference each other. An important part of the evaluation mechanism is
    how to follow these references.
  - **tagless** because all heap values - unevaluated values, functions, already
    evaluated values - are represented alike on the heap, in form of closures.
    Tag*ful* would mean these closures have to be annotated with things like
    type information, or whether they were previously evaluated already.
  - **graph reducing** because heap objects can be overwritten by simpler values
    the machine has found out to be equivalent. For example, the computation
    `1+1` on the heap might be overwritten by a constant `2` once that result
    has been obtained somewhere.


Useful applications
-------------------

STGi was started to teach myself about the STG. Not long into the project, I
decided to extend it to save others the many detours I had to take to implement
it. In that sense, it can be a useful tool if you're interested in the
lower-level properties of a Haskell implementation. I did my best to keep the
code readable, and added some decent Haddock/comment coverage. Speaking of
Haddock: it's an excellent tool to start looking around the project before
digging into the source!

The other benefit is for teaching others: instead (or in addition to!) of
explaining certain common Haskell issues on a whiteboard with boxes and arrows,
you can share an interactive view of common programs with others. The example
programs feature some interesting cases.

1. Does this leak memory? On the stack or the heap?
2. I heard GHC doesn't have a call stack?!
3. Why is this value not garbage collected?
4. Why are lists sometimes not very performant?
5. How many steps does this small, innocent function take to produce a result?


Language introduction
---------------------

The STG language can be seen as a mostly simplified version of Haskell with a
couple of lower level additions. The largest difference is probably that STG is
an untyped language.

The syntax will be discussed below. For now, as an appetizer, the familiar
Haskell code

```haskell
foldl' _ acc [] = acc
foldl' f acc (y:ys) = case f acc y of
	!acc' -> foldl' f acc' ys

sum = foldl' add 0
```

could be translated to

```haskell
foldl' = \f acc xs -> case xs of
    Nil -> acc;
    Cons y ys -> case f acc y of
        acc' -> foldl' f acc' ys;
    badList -> Error_foldl' badList;

sum = \ -> foldl' add zero;

zero = \ -> Int# 0#
```


### Top-level

An STG program consists of a set of bindings, each of the form

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

### The `main` value, termination

In the default configuration, program execution starts by moving the definitions
given in the source code onto the heap, and then evaluating the `main` value. It
will continue to run until there is no rule applicable to the current state. Due
to the lazy IO implementation, you can load indefinitely running programs in
your pager application and step as long forward as you want.

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

    and can be either all algebraic or all primitive. In case of algebraic
    alternatives, the constructor's arguments are in scope in the following
    expression, just like in Haskell's pattern matching.

    Each list of alts must include a default alternative at the end, which can
    optinally bind a variable.

    ```haskell
    v -> <expression>       -- bound default; v is in scope in the expression
    default -> <expression> -- unbound default
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
maybe = \nothing just x -> case x of
    Just j   -> just j;
    Nothing  -> nothing;
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
  tradeoff was chosen to keep the project's code simpler, but this may change
  in the future.

  For now, the semicolon rule is that **bindings and alternatives are
  semicolon-separated**.

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

  is invalid, and the machine would halt in an error state. You'll notice that
  the unboxing-boxing business is quite laborious, and this is precisely the
  reason unboxed values alone are so fast in GHC.

- Function application cannot be nested, since function arguments are primitives
  or variables. Haskell's `map f (map g xs)` would be written

  ```haskell
  let map_g_xs = \ -> map g xs
  in map f map_g_xs
  ```

  assuming all variables are in global scope. This means that nesting functions
  in Haskell results in a heap allocation via `let`.

- Free variable values have to be explicitly given to closures. Function
  composition could be implemented like

  ```haskell
  compose = \f g x -> let gx = \(g x) -> g x
                      in f gx
  ```

  Forgetting to hand `g` and `x` to the `gx` lambda form would mean that in the
  `g x` call neither of them was in scope, and the machine would halt with
  a "variable not in scope" error.

  This applies even for recursive functions, which have to be given to
  their own list of free variables, like in `rep` in the following example:

  ```haskell
  replicate = \x -> let rep = \(rep x) -> Cons x rep
  				    in rep
  ```


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
```

You can find lots of further examples of standard Haskell functions implemented
by hand in STG in the `Prelude` modules. Combined with the above explanations,
this is all you should need to get started.



### Marshalling values

The `Stg.Marshal` module provides functions to inject Haskell values into the
STG (`toStg`), and extract them from a machine state again (`toStg`). These
functions are tremendously useful in practice, make use of them! After chasing a
list value on the heap manually you'll know the value of `fromStg`, and in order
to get data structures into the STG you have to write a lot of code, and be
careful doing it at that. Keep in mind that `fromStg` requires the value to  be
in normal form, or extraction will fail.



Runtime behaviour
-----------------

The following steps are an overview of the evaluation rules. Running the STG in
verbose mode (`-v2`) will provide a more detailed description of what happened
each particular step.

### Code segment

The code segment is the current instruction the machine evaluates.

- **Eval** evaluates expressions.
    - **Function application** pushes the function's arguments on the stack
      and **Enter**s the address of the function.
	- **Constructor applications** simply transition into the
	  **ReturnCon** state when evaluated.
	- Similarly, **primitive ints** transition into the **ReturnInt** state.
	- **Case** pushes a return frame, and proceeds evaluating the scrutinee.
	- **Let(rec)** allocates heap closures, and extends the local environment
	  with the new bindings.
- **Enter** evaluates memory addresses by looking up the value at a memory
  address on the heap, and evaluating its body.
  	- If the closure entered is updatable, push an update frame so it can later
	  be overwritten with the value it evaluates to.
	- If the closure takes any arguments, supply it with values taken from
	  argument frames.
- **ReturnCon** instructs the machine to branch depending on which constructor
  is present, by popping a return frame.
- **ReturnInt** does the same, but for primitive values.

### Stack

The stack has three different kinds of frames.

- **Argument** frames store pending function arguments. They are pushed when
  evaluating a function applied to arguments, and popped when entering a closure
  that has a non-empty argument list.
- **Return** frames are pushed when evaluating a `case` expression, in order to
  know where to continue once the scrutinee has been evaluated. They are popped
  when evaluating constructors or primitive values.
- **Update** frames block access to argument and return frames. If an evaluation
  step needs to pop one of them but there is an update frame in the way, it can
  get rid the update frame by overriding the memory address pointed to by it
  with the current value being evaluated, and retrying the evaluation now that
  the update frame is gone. This mechanism is what enables lazy evaluation in
  the STG.

### Heap

The heap is a mapping from memory addresses to heap objects, which can be
closures or black holes (see below). Heap entries are allocated by `let(rec)`,
and deallocated by garbage collection.

As a visual guide to the user, closures are annotated with `Fun` (takes
arguments), `Con` (data constructors), and `Thunk` (suspended computations).


### Black holes

The heap does not only contain closures, but also black holes. Black holes are
annotated with the step in which they were created; this annotation is purely
for display purposes, and not used by the machine.

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


### Garbage collection

Currently, two garbage collection algorithms are implemented:

- **Tri-state tracing**: free all unused memory addresses, and does not touch
  the others. This makes following specific closures on the heap easy.
- **Two-space copying**: move all used memory addresses to the beginning of the
  heap, and discard all those that weren't moved. This has the advantage of
  reordering the heap roughly in the order the closures will be accessed by the
  program again, but the disadvantage of making things harder to track, since
  for example the `main` value might appear in several different locations
  throughout the run of a program.


### Unhelpful error message?

The goal of this project is being useful to human readers. If you find an error
message that is unhelpful or even misleading, please open an issue with a
minimal example on how to reproduce it!



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
- Although heap closures are all represented alike, they are classified for the
  user in the visual output:
    - Constructors are closures with a constructor application body, and
      only free variables.
    - Other closures with only free variables are thunks.
    - Closures with non-empty argument lists are functions.



GHC's current STG
-----------------

The implementation here uses the *push/enter* evaluation model of the STG, which
is fairly elegant, and was initially thought to also be top in terms of
performance. As it turned out, the latter is not the case, and another
evaluation model called *eval/apply*, which treats (only) function application a
bit different, is faster in practice.

This notable revision is documented in [the 2004 paper *How to make a fast
curry*][fastcurry]. I don't have plans to support this evaluation model right
now, but it's on my list of long-term goals (alongside the current push/enter).



[cabal]: https://www.haskell.org/cabal/
[fastcurry]: http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/
[ghc]: https://www.haskell.org/ghc/
[stack]: http://haskellstack.org/
[stg1992]: http://research.microsoft.com/apps/pubs/default.aspx?id=67083
