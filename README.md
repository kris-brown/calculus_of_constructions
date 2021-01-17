<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/calculus_of_constructions:/test/usr maltegruber/readme-tex:1.0.0

    see: https://github.com/MalteGruber/readme-tex

-->



# calculus_of_constructions

## Background: What is CoC

The CoC provides a language with which we can express mathematical _terms_ (e.g. ![3](doc/teximg/tex_img_0_SSLH5.png), ![3x+y](doc/teximg/tex_img_1_4NL9N.png), ![\overset{lim}{x \rightarrow 0}\frac{sin x}{x}](doc/teximg/tex_img_2_0FDOV.png)) and _types_ (e.g. the real numbers ![\mathbb{R}](doc/teximg/tex_img_3_DMZ0X.png), the type of functions ![\mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_4_SN9EK.png), the type of lists of length ![3](doc/teximg/tex_img_5_9BDR7.png) - note, it also turns out each type is itself a term of some other type).

The basic CoC syntax

The power of the CoC comes from a set of accompanying rules which allow us to construct terms of a certain type, i.e. prove that a term has a certain type.

## Why is it interesting

If the basic thing we can do is show that term ![t](doc/teximg/tex_img_6_3ZW6H.png) has type ![A](doc/teximg/tex_img_7_VLISN.png), it may be confusing as to what is the interest in this language at all, since the fact that ![\pi: \mathbb{R}](doc/teximg/tex_img_8_8SFMZ.png) or ![\lambda x, cos(x): \mathbb{R} \rightarrow \mathbb{R}](doc/teximg/tex_img_9_8NLDX.png) are not too remarkable. What we care about are more elaborate _propositions_ we can make about these mathematical terms, such as ![\forall x: \mathbb{R}, sin^2x+cos^2x=1](doc/teximg/tex_img_10_Q737M.png), or more abstract things, e.g. for the type of pairs of ![A \times B](doc/teximg/tex_img_11_K7HB0.png) there exists a _unique_ pair of projection functions ![A \xleftarrow{\pi_1} A \times B \xrightarrow{\pi_2} B](doc/teximg/tex_img_12_GBAT4.png) such that for _any_ pair of functions ![A \xleftarrow{f_A} X \xrightarrow{f_B} B](doc/teximg/tex_img_13_3CR1M.png), there is a unique map ![X \xrightarrow{f_!} A\times B](doc/teximg/tex_img_14_CZK51.png) such that ![f_!\pi_A = f_A](doc/teximg/tex_img_15_XAHVZ.png) and ![f_!\pi_B=f_b](doc/teximg/tex_img_16_N0OP2.png) (i.e. one can always _factor_ out a ![\pi](doc/teximg/tex_img_17_WAT4G.png) component from the functions). With the right interpretations, we can consider proofs that a certain compiler optimization does not alter the meaning of the unoptimized code or that a critical piece of code has no bugs.

It turns out our simple typing judgments are capable of proving such kinds of sweeping mathematical statements, due to a remarkable correspondance (called the Curry Howard Isomorphism) between the above language and the language of logic. Logical operations such as _and_, _or_, _for all_, _not_, etc. each have analogues which interact identically. Other than learning the exact translations for each of these operations, the key insight is to think of propositions as a type (called ![Prop](doc/teximg/tex_img_18_OA5CW.png)) that can have terms which are proofs of that proposition. Two example translation: the logical relation ![A \land B](doc/teximg/tex_img_19_PQ2S9.png) (![A](doc/teximg/tex_img_20_QKMFS.png) _and_ ![B](doc/teximg/tex_img_21_46UPL.png)) can be proved if and only if we have a proof of ![A](doc/teximg/tex_img_22_XZJBG.png) and a proof of ![B](doc/teximg/tex_img_23_6QUSK.png), which is tantamount to having a term of the type of pairs ![A \times B](doc/teximg/tex_img_24_2S4JU.png). Likewise, the logical relation ![A \implies B](doc/teximg/tex_img_25_M8YNB.png) (_if_ ![A](doc/teximg/tex_img_26_L7Q47.png), _then_ ![B](doc/teximg/tex_img_27_SH8ET.png)) is tantamount to a function type ![A \rightarrow B](doc/teximg/tex_img_28_UQR93.png), which provides a proof of ![B](doc/teximg/tex_img_29_BS026.png) if you feed it a proof of ![A](doc/teximg/tex_img_30_Q53SV.png).

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
