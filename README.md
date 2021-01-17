<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/calculus_of_constructions:/test/usr maltegruber/readme-tex:1.0.0;

    see: https://github.com/MalteGruber/readme-tex

-->



# calculus_of_constructions

## Background: What is CoC

The CoC provides a language with which we can express mathematical _terms_ (e.g. ![3](doc/teximg/tex_img_0_6AFZS.png), ![3x+y](doc/teximg/tex_img_1_KWCTC.png), ![\underset{x \rightarrow 0}{lim}\frac{sin(x)}{x}](doc/teximg/tex_img_2_QGOA1.png)) and _types_ (e.g. the real numbers ![\mathbb{R}](doc/teximg/tex_img_3_KAKFS.png), the type of functions ![\mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_4_NLO9R.png), the type of lists of length ![3](doc/teximg/tex_img_5_8UVHH.png) - note, it also turns out each type is itself a term of some other type).

The basic CoC syntax describes how to construct _pseudoterms_, which will represent types and terms (although some may be nonsensical).

- Variables, e.g. ![x,y,z](doc/teximg/tex_img_6_XABIX.png)
- Lambda abstractions
  - describe functions that substitute a variable into a body expression, e.g.:
    - the doubling function ![\lambda x, x+x](doc/teximg/tex_img_7_ET7TI.png)
    - the identity function ![\lambda z, z](doc/teximg/tex_img_8_FTL2U.png)
    - a function which takes two arguments and applies the first to the second: ![\lambda x, (\lambda f, f x)](doc/teximg/tex_img_9_G2T7Y.png).
- Pi types
  - represent the types of functions
  - ![\Pi x:A, B](doc/teximg/tex_img_10_MBE4I.png) refers to the type of functions that take a term of type ![A](doc/teximg/tex_img_11_LQQKE.png) and return a term of type ![B](doc/teximg/tex_img_12_43AYO.png) (which may or may not depend on the value of the input, ![x](doc/teximg/tex_img_13_N9H8H.png))). E.g.:
    - ![\Pi x:Int, List (x+x)](doc/teximg/tex_img_14_KW3ZM.png) accepts integers and returns elements of the type of lists of length ![x+x](doc/teximg/tex_img_15_4H4XU.png).
  - Often there's no dependence on the ![x](doc/teximg/tex_img_16_485ZC.png) at all, so we abbreviate with notation ![A \rightarrow B](doc/teximg/tex_img_17_GZYI8.png).
- The word _sort_ refers to the type of a type. In the flavor of CoC we implement here, we have as axioms the existence of two 'ground level' sorts ![Prop](doc/teximg/tex_img_18_LTYEP.png) and ![Set](doc/teximg/tex_img_19_6CHCL.png), as well as an infinite sequence higher order ![Type_i](doc/teximg/tex_img_20_YU8BU.png) for any natural number ![i](doc/teximg/tex_img_21_W2LLT.png).

The power of the CoC comes from a set of accompanying rules which allow us to construct terms of a certain type, i.e. prove a judgment that a term has a certain type. Notationally we write this as ![\{assumptions\} \vdash term : type](doc/teximg/tex_img_22_ZADXE.png). These rules capture the meanings of the symbols described above, so that we can prove things like ![x:\mathbb{R} \vdash (\lambda y: Set, x) : Set \rightarrow \mathbb{R}](doc/teximg/tex_img_23_WAEH3.png) (a lambda expression which accepts a set ![y](doc/teximg/tex_img_24_X6X72.png), ignores it and returns a constant ![x](doc/teximg/tex_img_25_XTOEG.png) of type ![\mathbb{R}](doc/teximg/tex_img_26_0MCCO.png) has the type of ![Set \rightarrow \mathbb{R}](doc/teximg/tex_img_27_V3HK3.png)).

## Why is it interesting

If the basic thing we can do is show that term ![t](doc/teximg/tex_img_28_1MDU9.png) has type ![A](doc/teximg/tex_img_29_LEIL2.png), it may be confusing as to what's the interest in this language at all, since the fact that ![\pi: \mathbb{R}](doc/teximg/tex_img_30_LE1CQ.png) or ![\lambda x, cos(x): \mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_31_14A9N.png) are not too remarkable. What we care about are more elaborate _propositions_ we can make about these mathematical terms, such as:

- ![\forall x: \mathbb{R}, sin^2x+cos^2x=1](doc/teximg/tex_img_32_BAJ00.png)
- for the type of _pairs_ of ![A \times B](doc/teximg/tex_img_33_NIRUA.png)
  - there exists a _unique_ pair of projection functions ![A \xleftarrow{\pi_1} A \times B \xrightarrow{\pi_2} B](doc/teximg/tex_img_34_4R7C5.png) such that for _any_ pair of functions ![A \xleftarrow{f_A} X \xrightarrow{f_B} B](doc/teximg/tex_img_35_U8PJV.png), there is a unique map ![X \xrightarrow{f_!} A\times B](doc/teximg/tex_img_36_TZM8E.png) such that ![f_!\pi_A = f_A](doc/teximg/tex_img_37_F40MR.png) and ![f_!\pi_B=f_b](doc/teximg/tex_img_38_0EA5W.png) (i.e. one can always _factor_ out a ![\pi](doc/teximg/tex_img_39_32ALI.png) component from the functions).
- With the right interpretations, we can consider proofs that a certain compiler optimization does not alter the meaning of the unoptimized code or that a critical piece of code has no bugs.

It turns out our simple typing judgments _are_ capable of proving such kinds of sweeping mathematical statements, due to a remarkable correspondance (called the [Curry Howard Isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)) between the above language and the language of logic. Logical operations such as _and_, _or_, _for all_, _not_, etc. each have analogues which behave just as they do in logic.

- Other than learning the exact translations for each of these operations, the key insight is to think of propositions as a type (called ![Prop](doc/teximg/tex_img_40_24PW1.png)) that can have terms which are proofs of that proposition.
- The logical relation ![A \land B](doc/teximg/tex_img_41_7GU6Q.png) (![A](doc/teximg/tex_img_42_6OBFL.png) _and_ ![B](doc/teximg/tex_img_43_4DLXF.png)) can be proved if and only if we have a proof of ![A](doc/teximg/tex_img_44_OLAYF.png) and a proof of ![B](doc/teximg/tex_img_45_4NWLC.png), which is tantamount to having a term of the type of pairs ![A \times B](doc/teximg/tex_img_46_0163P.png).
- The logical relation ![A \implies B](doc/teximg/tex_img_47_OLO6Z.png) (_if_ ![A](doc/teximg/tex_img_48_SEFG4.png), _then_ ![B](doc/teximg/tex_img_49_SJIXN.png)) is tantamount to a function type ![A \rightarrow B](doc/teximg/tex_img_50_PQPIZ.png), which provides a proof of ![B](doc/teximg/tex_img_51_OERW8.png) if you feed it a proof of ![A](doc/teximg/tex_img_52_HTTZS.png).

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
