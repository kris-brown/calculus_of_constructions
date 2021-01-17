<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/calculus_of_constructions:/test/usr maltegruber/readme-tex:1.0.0;

    see: https://github.com/MalteGruber/readme-tex

-->



# calculus_of_constructions

## Background: What is CoC

The CoC provides a language with which we can express mathematical _terms_ (e.g. ![3](doc/teximg/tex_img_0_PJSRZ.png), ![3x+y](doc/teximg/tex_img_1_3DUSL.png), ![\overset{lim}{x \rightarrow 0}\frac{sin x}{x}](doc/teximg/tex_img_2_3Y4SM.png)) and _types_ (e.g. the real numbers ![\mathbb{R}](doc/teximg/tex_img_3_GAFVJ.png), the type of functions ![\mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_4_7GJL8.png), the type of lists of length ![3](doc/teximg/tex_img_5_HY2D0.png) - note, it also turns out each type is itself a term of some other type).

The basic CoC syntax

The power of the CoC comes from a set of accompanying rules which allow us to construct terms of a certain type, i.e. prove that a term has a certain type.

## Why is it interesting

If the basic thing we can do is show that term ![t](doc/teximg/tex_img_6_S0X9P.png) has type ![A](doc/teximg/tex_img_7_UKB2G.png), it may be confusing as to what is the interest in this language at all, since the fact that ![\pi: \mathbb{R}](doc/teximg/tex_img_8_O4GJ1.png) or ![\lambda x, cos(x): \mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_9_T960U.png) are not too remarkable. What we care about are more elaborate _propositions_ we can make about these mathematical terms, such as ![\forall x: \mathbb{R}, sin^2x+cos^2x=1](doc/teximg/tex_img_10_BNSS1.png), or more abstract things, e.g. for the type of pairs of ![A \times B](doc/teximg/tex_img_11_WQ5S1.png) there exists a _unique_ pair of projection functions ![A \xleftarrow{\pi_1} A \times B \xrightarrow{\pi_2} B](doc/teximg/tex_img_12_ZUWG1.png) such that for _any_ pair of functions ![A \xleftarrow{f_A} X \xrightarrow{f_B} B](doc/teximg/tex_img_13_LRWJK.png), there is a unique map ![X \xrightarrow{f_!} A\times B](doc/teximg/tex_img_14_R7HDX.png) such that ![f_!\pi_A = f_A](doc/teximg/tex_img_15_KB7H6.png) and ![f_!\pi_B=f_b](doc/teximg/tex_img_16_CB2UA.png) (i.e. one can always _factor_ out a ![\pi](doc/teximg/tex_img_17_EVLMY.png) component from the functions). With the right interpretations, we can consider proofs that a certain compiler optimization does not alter the meaning of the unoptimized code or that a critical piece of code has no bugs.

It turns out our simple typing judgments are capable of proving such kinds of sweeping mathematical statements, due to a remarkable correspondance (called the Curry Howard Isomorphism) between the above language and the language of logic. Logical operations such as _and_, _or_, _for all_, _not_, etc. each have analogues which interact identically. Other than learning the exact translations for each of these operations, the key insight is to think of propositions as a type (called ![Prop](doc/teximg/tex_img_18_2YWAW.png)) that can have terms which are proofs of that proposition. Two example translation: the logical relation ![A \land B](doc/teximg/tex_img_19_12RQP.png) (![A](doc/teximg/tex_img_20_XFMLG.png) _and_ ![B](doc/teximg/tex_img_21_BWCSL.png)) can be proved if and only if we have a proof of ![A](doc/teximg/tex_img_22_OXIL6.png) and a proof of ![B](doc/teximg/tex_img_23_TBWQJ.png), which is tantamount to having a term of the type of pairs ![A \times B](doc/teximg/tex_img_24_W6RQI.png). Likewise, the logical relation ![A \implies B](doc/teximg/tex_img_25_61MN2.png) (_if_ ![A](doc/teximg/tex_img_26_1T644.png), _then_ ![B](doc/teximg/tex_img_27_LFXV7.png)) is tantamount to a function type ![A \rightarrow B](doc/teximg/tex_img_28_05PBC.png), which provides a proof of ![B](doc/teximg/tex_img_29_S1E3O.png) if you feed it a proof of ![A](doc/teximg/tex_img_30_COZVY.png).

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
