<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/calculus_of_constructions:/test/usr maltegruber/readme-tex:1.0.0;

    see: https://github.com/MalteGruber/readme-tex

-->



# calculus_of_constructions

## Background: What is CoC

The CoC provides a language with which we can express mathematical _terms_ (e.g. ![3](doc/teximg/tex_img_0_ZWIHP.png), ![3x+y](doc/teximg/tex_img_1_3FPUD.png), ![\underset{x \rightarrow 0}{lim}\frac{sin(x)}{x}](doc/teximg/tex_img_2_I7OCZ.png)) and _types_ (e.g. the real numbers ![\mathbb{R}](doc/teximg/tex_img_3_5OK1W.png), the type of functions ![\mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_4_X84G9.png), the type of lists of length ![3](doc/teximg/tex_img_5_26I44.png) - note, it also turns out each type is itself a term of some other type).

The basic CoC syntax describes how to construct _pseudoterms_, which will represent types and terms (although some may be nonsensical).

- Variables, e.g. ![x,y,z](doc/teximg/tex_img_6_VXQ57.png)
- Lambda abstractions
  - describe functions that substitute a variable into a body expression, e.g.:
    - the doubling function ![\lambda x, x+x](doc/teximg/tex_img_7_0CYZG.png)
    - the identity function ![\lambda z, z](doc/teximg/tex_img_8_U3WSO.png)
    - a function which takes two arguments and applies the first to the second: ![\lambda x, (\lambda f, f x)](doc/teximg/tex_img_9_OXZO9.png).
- Pi types
  - represent the types of functions
  - ![\Pi x:A, B](doc/teximg/tex_img_10_4K9OX.png) refers to the type of functions that take a term of type ![A](doc/teximg/tex_img_11_TZGWS.png) and return a term of type ![B](doc/teximg/tex_img_12_X01HG.png) (which may or may not depend on the value of the input, ![x](doc/teximg/tex_img_13_QPZW5.png))). E.g.:
    - ![\Pi x:Int, List (x+x)](doc/teximg/tex_img_14_7XJQN.png) accepts integers and returns elements of the type of lists of length ![x+x](doc/teximg/tex_img_15_STLXU.png).
  - Often there's no dependence on the ![x](doc/teximg/tex_img_16_9JCAE.png) at all, so we abbreviate these cases with notation ![A \rightarrow B](doc/teximg/tex_img_17_W0MOO.png).
  - Given the meaning of `\Pi`, it is often written as `\forall` (which means "for all").
- The word _sort_ refers to the type of a type. In the flavor of CoC we implement here, we have as axioms the existence of two 'ground level' sorts ![Prop](doc/teximg/tex_img_18_AT6OZ.png) and ![Set](doc/teximg/tex_img_19_G08H6.png), as well as an infinite sequence higher order sorts ![Type_i](doc/teximg/tex_img_20_X92LD.png) for any natural number ![i](doc/teximg/tex_img_21_NHPU6.png).

The power of the CoC comes from a set of accompanying rules which allow us to construct terms of a certain type, i.e. prove a judgment that a term has a certain type (and that the term is well-formed). Notationally we write this as ![\{assumptions\} \vdash term : type](doc/teximg/tex_img_22_URO8H.png). These rules capture the meanings of the symbols described above, so that we can prove things like ![\{x:\mathbb{R}\} \vdash (\lambda y: Set, x) : Set \rightarrow \mathbb{R}](doc/teximg/tex_img_23_EITJR.png) (i.e. a lambda expression which accepts a set but ignores it and returns a constant ![x](doc/teximg/tex_img_24_N9VWT.png) of type ![\mathbb{R}](doc/teximg/tex_img_25_KPZPO.png) has the type of ![Set \rightarrow \mathbb{R}](doc/teximg/tex_img_26_J8KZ1.png)).

## Why is it interesting

If the basic thing we can do is show that term ![t](doc/teximg/tex_img_27_0PA32.png) has type ![A](doc/teximg/tex_img_28_6UGX3.png), it may be confusing as to what's the interest in this language at all, since the fact that ![\pi: \mathbb{R}](doc/teximg/tex_img_29_Z2W1K.png) or ![\lambda x, cos(x): \mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_30_FVPNW.png) are not too remarkable. What we care about are more elaborate _propositions_ we can make about these mathematical terms, such as:

- ![\forall x: \mathbb{R}, sin^2x+cos^2x=1](doc/teximg/tex_img_31_JNO63.png)
- for the type of _pairs_ of ![A \times B](doc/teximg/tex_img_32_FNNJS.png)
  - there exists a _unique_ pair of projection functions ![A \xleftarrow{\pi_1} A \times B \xrightarrow{\pi_2} B](doc/teximg/tex_img_33_GA0IU.png) such that for _any_ pair of functions ![A \xleftarrow{f_A} X \xrightarrow{f_B} B](doc/teximg/tex_img_34_SILMZ.png), there is a unique map ![X \xrightarrow{f_!} A\times B](doc/teximg/tex_img_35_BQYP1.png) such that ![f_!\pi_A = f_A](doc/teximg/tex_img_36_D8JAT.png) and ![f_!\pi_B=f_b](doc/teximg/tex_img_37_E7F5A.png) (i.e. one can always _factor_ out a ![\pi](doc/teximg/tex_img_38_0CD6F.png) component from the functions).
- A certain compiler optimization does not alter the meaning of the unoptimized code, or a critical piece of code has no bugs.

It turns out our simple typing judgments _are_ capable of proving such kinds of sweeping mathematical statements, due to a remarkable correspondance (called the [Curry Howard Isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)) between the above language and the language of logic. Logical operations such as _and_, _or_, _for all_, _not_, etc. each have analogues which behave just as they do in logic.

- Other than learning the exact translations for each of these operations, the key insight is to think of propositions as a type (called ![Prop](doc/teximg/tex_img_39_SRX0Y.png)) that can have terms which are proofs of that proposition.
- The logical relation ![A \land B](doc/teximg/tex_img_40_10GVP.png) (![A](doc/teximg/tex_img_41_NS259.png) _and_ ![B](doc/teximg/tex_img_42_YN2NN.png)) can be proved if and only if we have a proof of ![A](doc/teximg/tex_img_43_GZU61.png) and a proof of ![B](doc/teximg/tex_img_44_0HSI0.png), which is tantamount to having a term of the type of pairs ![A \times B](doc/teximg/tex_img_45_5WREN.png).
- The logical relation ![A \implies B](doc/teximg/tex_img_46_ZXMXN.png) (_if_ ![A](doc/teximg/tex_img_47_UZ93H.png), _then_ ![B](doc/teximg/tex_img_48_WBVGZ.png)) is tantamount to a function type ![A \rightarrow B](doc/teximg/tex_img_49_4MD4R.png), which provides a proof of ![B](doc/teximg/tex_img_50_08MYD.png) if you feed it a proof of ![A](doc/teximg/tex_img_51_9QT4Z.png).

Continuing in this fashion, we can represent arbitrarily complex statements about arbitrarily complex mathematical structures and have a computer mechanically verify them._Discovering_ these proof terms is not a step that can be fully automated, though this is a field of active research.

## Inductive types

In this repo, we use some common extensions of CoC to make it more usable, most notably adding inductive types.

Although booleans, natural numbers, and other types can be defined with the above infrastructure (called Church encoding), inductive types allow for an efficient representation. These inductive types can be constructed by one of their custom constructors and eliminated by case analysis (e.g. a function `I -> X` for some inductive type `I` must be defined for all possible constructors of `I`). These are easiest explained through examples (which can be found in `data/base.txt`):

`Bool` can be modeled as an inductive type - i.e. something that can be `true` or `false` but not neither nor both.

```
Inductive Bool : Set :=
| tt : Bool
| ff : Bool.
```

The natural numbers are a prototypical example of inductive types, as any element of it is either zero or the successor to another natural number (thus, `zero` lets us constructor a `Nat` in any context, and the `succ` constructor behaves like a function of type `Nat → Nat`).

```
Inductive Nat : Set :=
| zero : Nat
| succ : (Nat → Nat).
```

`List`s and `vector`s force us to consider inductive types that have a parameter, i.e. `List` itself isn't a type but rather must be applied to some parameter (in this case, `A`, an arbitrary `Type`) to form a type. All of the constructors implicitly require this parameter too, in addition to other arguments (`nil B` lets us construct an empty `List B`, while an element `c:C` can be added to a list `l_c:List C` by calling `cons C c l_c` which produces another element of type `List C`).

```
Inductive List (A: Type) : Type :=
| nil : (List A)
| cons : (A → ((List A) → (List A))).
```

`Vector`s work very similarly, but keep track of how long the length of the list is by construction (the `Vnil X` case is forced to be of type `vector X 0` rather than any other number, and `Vcons` will always increment the type-level counter each time an element is appended).

```
Inductive vector (A: Type) : (Nat → Type) :=
| Vnil : (vector A zero)
| Vcons : (A → ((vector A n) → (vector A (succ n)))).
```

We now introduce the basics of "propositions as types". We model `True` (`⊤`/`Unit`/`()`) as an inductive type with a single constructor. In _any_ context we are justified in producing a term of this type, reflecting how trivial it is to 'prove' `True`.

```
Inductive True : Prop :=
| true_mk : True.
```

We model `False` (`⟂`/`Void`) as an inductive type with no constructors. It is not possible to construct a term of this type (i.e. _prove_ `False`) without being in a trivial context that can prove anything (this allows us to model `Not A` as `A -> False`, meaning `A` is absurd enough to prove anything).

```
Inductive False : Prop :=.
```

There are two constructors for `Or A B` to reflect _either_ a proof of `A` or a proof of `B` is sufficient to prove the disjunction, while `And A B` works as described above.

```
Inductive OR (A: Prop) (B: Prop) : Prop :=
| inl : (A → (OR A B))
| inr : (B → (OR A B)).

Inductive AND (A: Prop) (B: Prop) : Prop :=
| and_mk : (A → (B → (AND A B))).
```

As a final example, consider the proposition that `p ≤ q`. If less-than-or-equal-to (`le`) is defined as below, then we know it could have only been constructed from one of two scenarios:

- `p = 0`, in which case it's true because `0` is less than or equal to every natural number
- `p-1 ≤ q-1`, i.e. we have a proof that after peeling back one successor from each number that the property holds (we can keep doing this and reach zero eventually if and only if truely `p ≤ q`).

```
Inductive le: (Nat → Nat → Prop) :=
| le_n : (∀ (n: Nat), (le zero n))
| le_S : (∀ (n: Nat) (m: Nat), (le n m) -> le (succ n) (succ m)).
```

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

```

```
