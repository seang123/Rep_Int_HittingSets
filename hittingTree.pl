:- [diagnosis].



%call_prob1(_, _, _, [], [], OUT).
call_prob1(SD, COMP, OBS, HS, CS, OUT) :-
    % Calls problem 1 till all conflict sets are gotten.
    problem1(SD, COMP, OBS),
    tp(SD, COMP, OBS, [HS|OUT], CS),
    % TODO: if tp returns false return OUT.
    call_prob1(SD, COMP, OBS, [HS|CS], CS, [OUT|CS]).


call_problem1(SD, COMPS, OBS, HS, CS, OUT, OUT).
call_problem1(SD, COMPS, OBS, HS, CS, OUT, O) :-
    problem1(SD, COMP, OBS),
    append(HS, OUT, Z), % add the outputs together.
    tp(SD, COMP, OBS, Z, CS),


% delete all occurances of a given element from a list
% Works.
delSet(_, [], Res, Res).
delSet(Elem, [X|Xs], Res, R) :-
    Elem == X,
    delSet(Elem, Xs, Res, R)
    ;
    not(Elem == X),
    append(Res, [X], Z),
    delSet(Elem, Xs, Z, R).
