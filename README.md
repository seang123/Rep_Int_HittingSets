# Rep_Int_HittingSets
Implementation of the Hitting set algorithm.

---

This program calculates the minimal hitting sets for a given diagnostic problem.

problem1(SD, COMP, OBS), tp(SD, COMP, OBS, [], CS).

Here CS is the critical set, which is used to create the hitting sets.

Our code can extract a complete critical set from a given problem to be used in the hitting set algorithm, rather than continuously calling the tp function during the algorith to get the next CS. 
