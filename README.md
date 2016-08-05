# Aoba
A regex manipulation lib written in Haskell</br>
To compile: ghc  -O2  aoba.hs RE.hs </br>
Usage:</br>
./aoba -i re1 re2 -- calculate intersection set</br>
./aoba -c re1 re2 -- is re1 a subset of re2</br>
./aoba -e re1 re2 -- equality</br>

For detailed theory, please check out:</br>
Valentin M. Antimirov: Partial Derivatives of Regular Expressions and Finite Automaton Constructions. </br>
Theor. Comput. Sci. 155(2): 291-319 (1996)
