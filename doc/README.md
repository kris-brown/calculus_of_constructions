<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/calculus_of_constructions:/test/usr maltegruber/readme-tex:1.0.0;

    see: https://github.com/MalteGruber/readme-tex

-->

$@
\usepackage{amssymb}
\renewcommand{\vector}[1]{(x_1,x_2,\ldots,x_{#1})}
$@

# calculus_of_constructions

## Background: What is CoC

The CoC provides a language with which we can express mathematical _terms_ (e.g. $3$, $3x+y$, $\underset{x \rightarrow 0}{lim}\frac{sin(x)}{x}$) and _types_ (e.g. the real numbers $\mathbb{R}$, the type of functions $\mathbb{R} \rightarrow \mathbb{R}$, the type of lists of length $3$ - note, it also turns out each type is itself a term of some other type).

The basic CoC syntax describes how to construct _pseudoterms_, which will represent types and terms (although some may be nonsensical).

- Variables, e.g. $x,y,z$
- Lambda abstractions
  - describe functions that substitute a variable into a body expression, e.g.:
    - the doubling function $\lambda x, x+x$
    - the identity function $\lambda z, z$
    - a function which takes two arguments and applies the first to the second: $\lambda x, (\lambda f, f x)$.
- Pi types
  - represent the types of functions
  - $\Pi x:A, B$ refers to the type of functions that take a term of type $A$ and return a term of type $B$ (which may or may not depend on the value of the input, $x$)). E.g.:
    - $\Pi x:Int, List (x+x)$ accepts integers and returns elements of the type of lists of length $x+x$.
  - Often there's no dependence on the $x$ at all, so we abbreviate with notation $A \rightarrow B$.
- The word _sort_ refers to the type of a type. In the flavor of CoC we implement here, we have as axioms the existence of two 'ground level' sorts $Prop$ and $Set$, as well as an infinite sequence higher order $Type_i$ for any natural number $i$.

The power of the CoC comes from a set of accompanying rules which allow us to construct terms of a certain type, i.e. prove a judgment that a term has a certain type (and that the term is well-formed). Notationally we write this as $\{assumptions\} \vdash term : type$. These rules capture the meanings of the symbols described above, so that we can prove things like $\{x:\mathbb{R}\} \vdash (\lambda y: Set, x) : Set \rightarrow \mathbb{R}$ (i.e. a lambda expression which accepts a set but ignores it and returns a constant $x$ of type $\mathbb{R}$ has the type of $Set \rightarrow \mathbb{R}$).

## Why is it interesting

If the basic thing we can do is show that term $t$ has type $A$, it may be confusing as to what's the interest in this language at all, since the fact that $\pi: \mathbb{R}$ or $\lambda x, cos(x): \mathbb{R} \rightarrow \mathbb{R}$ are not too remarkable. What we care about are more elaborate _propositions_ we can make about these mathematical terms, such as:

- $\forall x: \mathbb{R}, sin^2x+cos^2x=1$
- for the type of _pairs_ of $A \times B$
  - there exists a _unique_ pair of projection functions $A \xleftarrow{\pi_1} A \times B \xrightarrow{\pi_2} B$ such that for _any_ pair of functions $A \xleftarrow{f_A} X \xrightarrow{f_B} B$, there is a unique map $X \xrightarrow{f_!} A\times B$ such that $f_!\pi_A = f_A$ and $f_!\pi_B=f_b$ (i.e. one can always _factor_ out a $\pi$ component from the functions).
- A certain compiler optimization does not alter the meaning of the unoptimized code, or a critical piece of code has no bugs.

It turns out our simple typing judgments _are_ capable of proving such kinds of sweeping mathematical statements, due to a remarkable correspondance (called the [Curry Howard Isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)) between the above language and the language of logic. Logical operations such as _and_, _or_, _for all_, _not_, etc. each have analogues which behave just as they do in logic.

- Other than learning the exact translations for each of these operations, the key insight is to think of propositions as a type (called $Prop$) that can have terms which are proofs of that proposition.
- The logical relation $A \land B$ ($A$ _and_ $B$) can be proved if and only if we have a proof of $A$ and a proof of $B$, which is tantamount to having a term of the type of pairs $A \times B$.
- The logical relation $A \implies B$ (_if_ $A$, _then_ $B$) is tantamount to a function type $A \rightarrow B$, which provides a proof of $B$ if you feed it a proof of $A$.

Continuing in this fashion, we can represent arbitrarily complex statements about arbitrarily complex mathematical structures and have a computer mechanically verify (_finding_ the proof terms is not a step that can be fully automated, though this is a field of active research).

## Inductive types

In this repo, we use some common extensions of CoC to make it more usable, most notably adding inductive types.

## Functionality

This repo has implementations in different languages.

|                    Language                    |           Python            |         Haskell          |
| :--------------------------------------------: | :-------------------------: | :----------------------: |
|                 Run test suite                 | ✅<br> Run `pytest test.py` | ✅ <br> Run `stack test` |
| Judge/typecheck pseudoterms to determine types |             ✅              |            ❌            |
|         Evaluate/β-reduce expressions          |             ✅              |            ❌            |
|  Pretty print and parse files of expressions   |             ✅              |            ❌            |

## References

1. TODO
