<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/calculus_of_constructions:/test/usr maltegruber/readme-tex:1.0.0;

    see: https://github.com/MalteGruber/readme-tex

-->



# calculus_of_constructions

## Background: What is CoC

The CoC provides a language with which we can express mathematical _terms_ (e.g. ![3](doc/teximg/tex_img_0_YI9QG.png), ![3x+y](doc/teximg/tex_img_1_56DG3.png), ![\underset{x \rightarrow 0}{lim}\frac{sin(x)}{x}](doc/teximg/tex_img_2_5NSOB.png)) and _types_ (e.g. the real numbers ![\mathbb{R}](doc/teximg/tex_img_3_7YX00.png), the type of functions ![\mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_4_BF1ND.png), the type of lists of length ![3](doc/teximg/tex_img_5_SH6DT.png) - note, it also turns out each type is itself a term of some other type).

The basic CoC syntax describes how to construct _pseudoterms_, which will represent types and terms (although some may be nonsensical).

- Variables, e.g. ![x,y,z](doc/teximg/tex_img_6_BMJ5F.png)
- Lambda abstractions
  - describe functions that substitute a variable into a body expression, e.g.:
    - the doubling function ![\lambda x, x+x](doc/teximg/tex_img_7_RWQRI.png)
    - the identity function ![\lambda z, z](doc/teximg/tex_img_8_0P6VF.png)
    - a function which takes two arguments and applies the first to the second: ![\lambda x, (\lambda f, f x)](doc/teximg/tex_img_9_2LZTC.png).
- Pi types
  - represent the types of functions
  - ![\Pi x:A, B](doc/teximg/tex_img_10_4Z5DN.png) refers to the type of functions that take a term of type ![A](doc/teximg/tex_img_11_SY3YV.png) and return a term of type ![B](doc/teximg/tex_img_12_NSF71.png) (which may or may not depend on the value of the input, ![x](doc/teximg/tex_img_13_5TX8O.png))). E.g.:
    - ![\Pi x:Int, List (x+x)](doc/teximg/tex_img_14_I9R8Q.png) accepts integers and returns elements of the type of lists of length ![x+x](doc/teximg/tex_img_15_1NOFC.png).
  - Often there's no dependence on the ![x](doc/teximg/tex_img_16_UT1N9.png) at all, so we abbreviate with notation ![A \rightarrow B](doc/teximg/tex_img_17_JLEY7.png).
- The word _sort_ refers to the type of a type. In the flavor of CoC we implement here, we have as axioms the existence of two 'ground level' sorts ![Prop](doc/teximg/tex_img_18_S600G.png) and ![Set](doc/teximg/tex_img_19_Y5MOA.png), as well as an infinite sequence higher order ![Type_i](doc/teximg/tex_img_20_HK4ES.png) for any natural number ![i](doc/teximg/tex_img_21_BKWWK.png).

The power of the CoC comes from a set of accompanying rules which allow us to construct terms of a certain type, i.e. prove a judgment that a term has a certain type (and that the term is well-formed). Notationally we write this as ![\{assumptions\} \vdash term : type](doc/teximg/tex_img_22_NBDK6.png). These rules capture the meanings of the symbols described above, so that we can prove things like ![\{x:\mathbb{R}\} \vdash (\lambda y: Set, x) : Set \rightarrow \mathbb{R}](doc/teximg/tex_img_23_GSAR9.png) (i.e. a lambda expression which accepts a set but ignores it and returns a constant ![x](doc/teximg/tex_img_24_072XE.png) of type ![\mathbb{R}](doc/teximg/tex_img_25_1CAQA.png) has the type of ![Set \rightarrow \mathbb{R}](doc/teximg/tex_img_26_YCTEP.png)).

## Why is it interesting

If the basic thing we can do is show that term ![t](doc/teximg/tex_img_27_L3P8W.png) has type ![A](doc/teximg/tex_img_28_NXE31.png), it may be confusing as to what's the interest in this language at all, since the fact that ![\pi: \mathbb{R}](doc/teximg/tex_img_29_NZTFG.png) or ![\lambda x, cos(x): \mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_30_95HWA.png) are not too remarkable. What we care about are more elaborate _propositions_ we can make about these mathematical terms, such as:

- ![\forall x: \mathbb{R}, sin^2x+cos^2x=1](doc/teximg/tex_img_31_JV4G0.png)
- for the type of _pairs_ of ![A \times B](doc/teximg/tex_img_32_MSIKM.png)
  - there exists a _unique_ pair of projection functions ![A \xleftarrow{\pi_1} A \times B \xrightarrow{\pi_2} B](doc/teximg/tex_img_33_AHAS9.png) such that for _any_ pair of functions ![A \xleftarrow{f_A} X \xrightarrow{f_B} B](doc/teximg/tex_img_34_8HHR9.png), there is a unique map ![X \xrightarrow{f_!} A\times B](doc/teximg/tex_img_35_WS8SI.png) such that ![f_!\pi_A = f_A](doc/teximg/tex_img_36_5HKQG.png) and ![f_!\pi_B=f_b](doc/teximg/tex_img_37_YNW52.png) (i.e. one can always _factor_ out a ![\pi](doc/teximg/tex_img_38_XBZLM.png) component from the functions).
- A certain compiler optimization does not alter the meaning of the unoptimized code, or a critical piece of code has no bugs.

It turns out our simple typing judgments _are_ capable of proving such kinds of sweeping mathematical statements, due to a remarkable correspondance (called the [Curry Howard Isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)) between the above language and the language of logic. Logical operations such as _and_, _or_, _for all_, _not_, etc. each have analogues which behave just as they do in logic.

- Other than learning the exact translations for each of these operations, the key insight is to think of propositions as a type (called ![Prop](doc/teximg/tex_img_39_U7QI6.png)) that can have terms which are proofs of that proposition.
- The logical relation ![A \land B](doc/teximg/tex_img_40_D3Z5R.png) (![A](doc/teximg/tex_img_41_JC7E3.png) _and_ ![B](doc/teximg/tex_img_42_2U7Z7.png)) can be proved if and only if we have a proof of ![A](doc/teximg/tex_img_43_04I55.png) and a proof of ![B](doc/teximg/tex_img_44_3WXFM.png), which is tantamount to having a term of the type of pairs ![A \times B](doc/teximg/tex_img_45_AL38V.png).
- The logical relation ![A \implies B](doc/teximg/tex_img_46_8TYOP.png) (_if_ ![A](doc/teximg/tex_img_47_4MY0C.png), _then_ ![B](doc/teximg/tex_img_48_J7NOB.png)) is tantamount to a function type ![A \rightarrow B](doc/teximg/tex_img_49_P8JR9.png), which provides a proof of ![B](doc/teximg/tex_img_50_6V4VZ.png) if you feed it a proof of ![A](doc/teximg/tex_img_51_XHFCO.png).

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
