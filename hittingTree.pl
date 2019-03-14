:- [diagnosis].


% ########################################################
% Gets the complete critical set for a diagnosis problem.
% Currently needs to be called till failure and then the last output can be taken.
% ?- call_problem1(SD, COMPS, OBS, [], CS, [], O)
call_problem1(SD, COMP, OBS, HS, CS, OUT, OUT).
call_problem1(SD, COMP, OBS, HS, CS, OUT, O) :-
    %write("HS = "), write(HS),
    problem1(SD, COMP, OBS),
    tp(SD, COMP, OBS, HS, CS),
    append(HS, CS, Z),
    call_problem1(SD, COMP, OBS, Z, CCS, [OUT|CS], O).

call_problem2(SD, COMP, OBS, HS, CS, OUT, OUT).
call_problem2(SD, COMP, OBS, HS, CS, OUT, O) :-
    %write("HS = "), write(HS),
    problem2(SD, COMP, OBS),
    tp(SD, COMP, OBS, HS, CS),
    append(HS, CS, Z),
    call_problem2(SD, COMP, OBS, Z, CCS, [OUT|CS], O).

call_problem3(SD, COMP, OBS, HS, CS, OUT, OUT).
call_problem3(SD, COMP, OBS, HS, CS, OUT, O) :-
    %write("HS = "), write(HS),
    problem3(SD, COMP, OBS),
    tp(SD, COMP, OBS, HS, CS),
    append(HS, CS, Z),
    call_problem3(SD, COMP, OBS, Z, CCS, [OUT|CS], O).

call_problem4(SD, COMP, OBS, HS, CS, OUT, OUT).
call_problem4(SD, COMP, OBS, HS, CS, OUT, O) :-
    %write("HS = "), write(HS),
    fulladder(SD, COMP, OBS),
    tp(SD, COMP, OBS, HS, CS),
    append(HS, CS, Z),
    call_problem4(SD, COMP, OBS, Z, CCS, [OUT|CS], O).
% ########################################################


% delete all occurances of a given element from a list
% ?- delSet(2, [1,2,3], [], R)
delSet(_, [], Res, Res).
delSet(Elem, [X|Xs], Res, R) :-
    Elem == X,
    delSet(Elem, Xs, Res, R)
    ;
    not(Elem == X),
    append(Res, [X], Z),
    delSet(Elem, Xs, Z, R).
