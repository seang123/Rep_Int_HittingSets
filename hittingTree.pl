:- [diagnosis, tp].
:- style_check(-singleton). % Hides singleton variable warning message

% finds the possible hitting sets for a diagnostics problem
solver(SD, COMP, OBS, HS, TREE, CS, TREE) :-
    not(tp(SD, COMP, OBS, HS, CS)).
solver(SD, COMP, OBS, HS, TREE, CS, OUT) :-
    tp(SD, COMP, OBS, HS, CS),
    member(X, CS),
    solver(SD, COMP, OBS, [X|HS], [X|TREE], CSS, OUT).

% used to call the solver and find all solution
solve(SD, COMP, OBS, X) :-
    findall(OUT, solver(SD, COMP, OBS, [], [], CS, OUT), Y),
    % call the pruning function
    testprune(Y, X).

% Prunes the hitting set solutions of supersets, and duplicates
prune(_, Acc, Acc) :-
    length(Acc, N),
    N > 0.
prune([L1|Ls], Acc, R) :-
        member(X, Ls),
        findall(Y, member(Y, Ls), YY),
        subset(X, L1) % L1 superset of X
    ->
        prune(Ls, [], R)
    ;
        prune(Ls, L1, R).

% runs the prune function on the hitting sets.
testprune(L, EE) :-
    findall(R, prune(L, [], R), RR),
    reverse(RR, W),
    findall(E, prune(W, [], E), EE).
