<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/calculus_of_constructions:/test/usr maltegruber/readme-tex:1.0.0;

    see: https://github.com/MalteGruber/readme-tex

-->



# calculus_of_constructions

## Background: What is CoC

The CoC provides a language with which we can express mathematical _terms_ (e.g. ![3](doc/teximg/tex_img_0_N4MH7.png), ![3x+y](doc/teximg/tex_img_1_OZHGD.png), ![\underset{x \rightarrow 0}{lim}\frac{sin(x)}{x}](doc/teximg/tex_img_2_CN680.png)) and _types_ (e.g. pairs of real numbers ![\mathbb{R} \times \mathbb{R}](doc/teximg/tex_img_3_C2096.png), the type of functions ![\mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_4_X6SCQ.png), the type of lists of length ![3](doc/teximg/tex_img_5_1J82R.png)).

The basic CoC syntax describes how to construct _pseudoterms_, which will represent types and terms.

- Variables, e.g. ![x,y,z](doc/teximg/tex_img_6_IPH8D.png)
- Applying a function to a term, e.g. `f(x)` (often written without parentheses, e.g. `plus n m`)
- Lambda abstractions
  - describe functions that substitute a variable into a body expression, e.g.:
    - the doubling function ![\lambda x, x+x](doc/teximg/tex_img_7_GBM6G.png)
    - the identity function ![\lambda z, z](doc/teximg/tex_img_8_6EIBE.png)
    - a function which takes two arguments and applies the first to the second: ![\lambda f, (\lambda x, f x)](doc/teximg/tex_img_9_32L8J.png).
- Pi types
  - represent the types of functions
  - ![\Pi x:A, B](doc/teximg/tex_img_10_TRIDM.png) refers to the type of functions that take a term of type ![A](doc/teximg/tex_img_11_T373M.png) and return a term of type ![B](doc/teximg/tex_img_12_8HRYA.png) (which may or may not depend on the value of the input, ![x](doc/teximg/tex_img_13_TXHT0.png))). E.g.:
    - ![\Pi x:Int, List (x+x)](doc/teximg/tex_img_14_4F90E.png) accepts integers and returns elements of the type of lists of length ![x+x](doc/teximg/tex_img_15_5Z70X.png).
  - Often there's no dependence on the ![x](doc/teximg/tex_img_16_B5OSZ.png) at all, so we abbreviate these cases with notation ![A \rightarrow B](doc/teximg/tex_img_17_J8TEU.png).
  - Given the meaning of ![\Pi](doc/teximg/tex_img_18_1S4LC.png), it is often written as ![\forall](doc/teximg/tex_img_19_IPENZ.png) (which means "for all").
- The word _sort_ refers to the type of a type. In the flavor of CoC we implement here, we have as axioms the existence of two 'ground level' sorts ![Prop](doc/teximg/tex_img_20_TEA7V.png) and ![Set](doc/teximg/tex_img_21_O0ZI1.png), as well as an infinite sequence higher order sorts ![Type_i](doc/teximg/tex_img_22_Y8Q8C.png) for any natural number ![i](doc/teximg/tex_img_23_IFHGN.png).

The power of the CoC comes from a set of accompanying rules which allow us to construct terms of a certain type. This firstly allows us to show that a term is not nonsense, e.g. treating a natural number as a function: ![1 + 3(1, 2)](doc/teximg/tex_img_24_VDV30.png). Furthermore, we can prove judgments saying a term has a certain type. Notationally we write this as ![\{assumptions\} \vdash term : type](doc/teximg/tex_img_25_2RTA9.png). These judgment-forming rules (described in the references below) capture the meanings of the symbols described above, so that we can prove things like:

- ![\{x:\mathbb{R}\} \vdash (\lambda y: Set, x) : Set \rightarrow \mathbb{R}](doc/teximg/tex_img_26_1H823.png)
- i.e. a lambda expression which accepts a set but ignores it and returns a constant ![x](doc/teximg/tex_img_27_SW6KL.png) of type ![\mathbb{R}](doc/teximg/tex_img_28_JTWR3.png) has the type of ![Set \rightarrow \mathbb{R}](doc/teximg/tex_img_29_KHC26.png).

## Why is it interesting

If the only thing we can do is show that term ![t](doc/teximg/tex_img_30_L7YLT.png) has type ![A](doc/teximg/tex_img_31_NUVS1.png), it may be confusing as to what's the interest in this language at all, since the fact that ![\pi: \mathbb{R}](doc/teximg/tex_img_32_8981F.png) or ![\lambda x, cos(x): \mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_33_XUUUJ.png) aren't that remarkable. What we care about are more elaborate _propositions_ we can make about these mathematical terms, such as:

- ![\forall x: \mathbb{R}, sin^2x+cos^2x=1](doc/teximg/tex_img_34_6E0VA.png)
- for the type of _pairs_ of ![A \times B](doc/teximg/tex_img_35_0WZ3E.png)
  - there exists a _unique_ pair of projection functions ![A \xleftarrow{\pi_1} A \times B \xrightarrow{\pi_2} B](doc/teximg/tex_img_36_R01FB.png) such that for _any_ pair of functions ![A \xleftarrow{f_A} X \xrightarrow{f_B} B](doc/teximg/tex_img_37_O8E2X.png), there is a unique map ![X \xrightarrow{f_!} A\times B](doc/teximg/tex_img_38_99E9S.png) such that ![f_!\pi_A = f_A](doc/teximg/tex_img_39_IMY1H.png) and ![f_!\pi_B=f_b](doc/teximg/tex_img_40_W370J.png) (i.e. one can always _factor_ out a ![\pi](doc/teximg/tex_img_41_ENECN.png) component from the functions).
- After encoding the relevant real-world entities into the CoC, we might want to prove certain compiler optimization does not alter the meaning of the unoptimized code, or that a critical piece of code has no bugs.

It turns out our simple typing judgments _are_ capable of proving such kinds of elaborate mathematical statements, due to a remarkable correspondance (called the [Curry Howard Isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)) between the above language and the language of logic. Logical operations such as _and_, _or_, _for all_, _not_, etc. each have analogues which behave just as they do in logic.

- Other than learning the exact translations for each of these operations, the key insight is to think of _propositions as types_ with _terms as proofs_ of that proposition.
- The logical relation ![A \land B](doc/teximg/tex_img_42_AZ52B.png) (![A](doc/teximg/tex_img_43_CMWRU.png) _and_ ![B](doc/teximg/tex_img_44_90CL9.png)) can be proved if and only if we have a proof of ![A](doc/teximg/tex_img_45_ED3EP.png) and a proof of ![B](doc/teximg/tex_img_46_KD8DO.png), which is tantamount to having a term of the type of pairs ![A \times B](doc/teximg/tex_img_47_WOXCB.png).
- The logical relation ![A \implies B](doc/teximg/tex_img_48_1M9GF.png) (_if_ ![A](doc/teximg/tex_img_49_8QGEG.png), _then_ ![B](doc/teximg/tex_img_50_D2LLK.png)) is tantamount to a function type ![A \rightarrow B](doc/teximg/tex_img_51_R49S5.png), which provides a proof of ![B](doc/teximg/tex_img_52_4HHZZ.png) if you feed it a proof of ![A](doc/teximg/tex_img_53_PCTQ3.png).

Continuing in this fashion, we can construct arbitrarily complex statements about arbitrarily complex mathematical structures and have a computer mechanically verify them. _Discovering_ these proof terms is not a step that can be fully automated, though this is a field of active research.

## Inductive types

In this repo, we use some common extensions of CoC to make it more usable, most notably adding inductive types.

Although booleans, natural numbers, and other types _can_ be defined (called the Church encoding) with the above infrastructure, inductive types allow for an efficient and intuitive representation. A term of an inductive type must be constructed by one of its custom constructors and eliminated by case analysis (e.g. a function `I -> X` for some inductive type `I` must be defined for all possible constructors of `I`). These are easiest explained through examples (which can be found in `data/base.txt`):

The type of Booleans, `Bool`, can be modeled as an inductive type - i.e. something that is either `true` or `false`.

```
Inductive Bool : Set :=
| tt : Bool
| ff : Bool.
```

The natural numbers are a prototypical example of inductive types, as any element of it is either zero or the successor to another natural number (thus, `zero` lets us constructor a `Nat` in any context, and the `succ` constructor behaves like a function of type `Nat → Nat`).

```
Inductive Nat : Set :=
| zero : Nat
| succ : Nat → Nat.
```

`Lists` and `vectors` force us to consider inductive types that have a parameter, i.e. `List` itself isn't a type but rather must be applied to some parameter (in this case, `A`, an arbitrary `Type`) to form a type. All of the constructors implicitly require this parameter too, in addition to other arguments (`nil B` lets us construct an empty `List B`, while an element `c:C` can be added to a list `l_c:List C` by calling `cons C c l_c` which produces another element of type `List C`).

```
Inductive List (A: Type) : Type :=
| nil : List A
| cons : A → List A → List A.
```

`Vectors` work very similarly, but keep track of how long the length of the list is at the type level (the `Vnil X` case is forced to be of type `vector X 0` rather than any other number, and `Vcons` will always increment the type-level counter each time an element is appended).

```
Inductive vector (A: Type) : (Nat → Type) :=
| Vnil : vector A zero
| Vcons : A → vector A n → vector A (succ n).
```

We now introduce the basics of "propositions as types" (which are distinct from the encoding of `true` and `false` as _values_ of type `Bool`). We model the _proposition_ `True` (AKA `⊤`/`Unit`/`()`) as an inductive type with a single constructor. In _any_ context we are justified in producing a term of this type, reflecting how trivial it is to 'prove' `True`.

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
| inl : A → OR A B
| inr : B → OR A B.

Inductive AND (A: Prop) (B: Prop) : Prop :=
| and_mk : A → B → AND A B.
```

As a final example, consider the proposition that `p ≤ q`. If less-than-or-equal-to (`le`) is defined as below, then we know it could have only been constructed from one of two scenarios:

- `p = 0`, in which case it's true because `0` is less than or equal to every natural number
- `p-1 ≤ q-1`, i.e. we have a proof that after peeling back one successor from each number that the property holds (we can keep doing this and reach zero eventually if and only if truely `p ≤ q`).

```
Inductive le: (Nat → Nat → Prop) :=
| le_n : ∀ (n: Nat), le zero n
| le_S : ∀ (n: Nat) (m: Nat), le n m -> le (succ n) (succ m).
```

In fact, knowing that these are the only two constructors allows us to prove that it's _not_ the case that `1 ≤ 0`.

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
