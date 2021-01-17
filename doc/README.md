<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/calculus_of_constructions:/test/usr maltegruber/readme-tex:1.0.0

    see: https://github.com/MalteGruber/readme-tex

-->

$@
\usepackage{amssymb}
\renewcommand{\vector}[1]{(x_1,x_2,\ldots,x_{#1})}
$@

# calculus_of_constructions

## Background: What is CoC

The CoC provides a language with which we can express mathematical _terms_ (e.g. $3$, $3x+y$, $\overset{lim}{x \rightarrow 0}\frac{sin x}{x}$) and _types_ (e.g. the real numbers $\mathbb{R}$, the type of functions $\mathbb{R} \rightarrow \mathbb{R}$, the type of lists of length $3$ - note, it also turns out each type is itself a term of some other type).

The basic CoC syntax

The power of the CoC comes from a set of accompanying rules which allow us to construct terms of a certain type, i.e. prove that a term has a certain type.

## Why is it interesting

If the basic thing we can do is show that term $t$ has type $A$, it may be confusing as to what is the interest in this language at all, since the fact that $\pi: \mathbb{R}$ or $\lambda x, cos(x): \mathbb{R} \rightarrow \mathbb{R}$ are not too remarkable. What we care about are more elaborate _propositions_ we can make about these mathematical terms, such as $\forall x: \mathbb{R}, sin^2x+cos^2x=1$, or more abstract things, e.g. for the type of pairs of $A \times B$ there exists a _unique_ pair of projection functions $A \xleftarrow{\pi_1} A \times B \xrightarrow{\pi_2} B$ such that for _any_ pair of functions $A \xleftarrow{f_A} X \xrightarrow{f_B} B$, there is a unique map $X \xrightarrow{f_!} A\times B$ such that $f_!\pi_A = f_A$ and $f_!\pi_B=f_b$ (i.e. one can always _factor_ out a $\pi$ component from the functions). With the right interpretations, we can consider proofs that a certain compiler optimization does not alter the meaning of the unoptimized code or that a critical piece of code has no bugs.

It turns out our simple typing judgments are capable of proving such kinds of sweeping mathematical statements, due to a remarkable correspondance (called the Curry Howard Isomorphism) between the above language and the language of logic. Logical operations such as _and_, _or_, _for all_, _not_, etc. each have analogues which interact identically. Other than learning the exact translations for each of these operations, the key insight is to think of propositions as a type (called $Prop$) that can have terms which are proofs of that proposition. Two example translation: the logical relation $A \land B$ ($A$ _and_ $B$) can be proved if and only if we have a proof of $A$ and a proof of $B$, which is tantamount to having a term of the type of pairs $A \times B$. Likewise, the logical relation $A \implies B$ (_if_ $A$, _then_ $B$) is tantamount to a function type $A \rightarrow B$, which provides a proof of $B$ if you feed it a proof of $A$.

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
