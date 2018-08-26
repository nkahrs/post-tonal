# post-tonal
a set of Haskell tools for basic calculations in post-tonal music theory

This repo contains some tools I wrote in a post-tonal theory class (TH402 at Eastman, Fall 2017, taught by Henry Klumpenhouwer) to automate some tedious exercises, and to gain insight into a few weird metatheoretical situations. I did this in Haskell as a way of making myself learn Haskell, and because it's a good fit for the group-theoretic flavor of post-tonal theory.

PostTonal.hs contains most stuff. ifunc is Lewin's interval function, TI is a container for a T/I operator, you can combine these operators and apply them to sets. primeForm calculates prime forms (but doesn't tell you Forte Numbers).

Hauptmann.hs implements Henry Klumpenhouwer's distinct appropriation of Hauptmann's theory of chord quality to post-tonal music, as discussed in his dissertation and as elaborated on in his course. I added some calculations to see how many sonorities you could possibly relate to one another.

Features not implemented (yet) include serial operations (involving rows) and K-nets.