# Representation & Interaction 
Implementation of the Hitting set algorithm.


This program calculates the minimal hitting sets for a given diagnostic problem.

---

The follling line will output the first conflict set of problem3.

`problem3(SD, COMP, OBS), tp(SD, COMP, OBS, [], CS).`

For each value in the output (CS) place it into the empty list (HS) and rerun the command.

If the output fails it means the branch is complete, otherwise the new output again needs to recursively have its values added to the HS list.

---

The following command will output all the hitting sets and optionally also prune the result to give the minimal hitting sets only.

`problem3(SD, COMP, OBS), solve(SD, COMP, OBS, Output).`

The pruning function removes duplicate elements, as well as elements that are supersets of other elements.
