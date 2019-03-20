:- [diagnosis].


% ########################################################
% Gets the different critical sets of a diagnostic problem
% ?- call_problem1(SD, COMPS, OBS, [], CS, [], OUTPUT)
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


print_set([X|XS]):-
  is_list(X) -> print_set(X).

pair_firsts(X, Y, Out):-
  get_first_element(X,A),
  get_first_element(Y,B),
  concatenate([A],[B], Out).

concatenate(A, B, Out):-
  A\==B -> append([A],[B],Out);
  Out = A.

get_first_element([X|Xs], Out):-
  is_list(X) ->  get_first_element(X, Out);
  Out = X.


  % write("reached_concatenate"), atom(X), atom(Y)->  append(X, Y, Out);
  % write("not equal"), append(X, Y, Out).

%(X \== Y) ,
%[[X1, X2],[X1, A2, O1]]

% [X1, X2],[X1, A2, O1], Out


  %write(print_set(X)),print_set(XS).

% Gets the complete critical set of a diagnostic problem.
get_problem1(X) :-
    findall(OUTPUT, call_problem1(SD, COMPS, OBS, [], CS, [], OUTPUT), O),
    last(O, X).
% ################# delete from set ######################

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

% ################## Intersection #######################
% ?- get_inter([1,2,3, [4, 5], [6, 7]], [5, [4, 5], [6]], X).
% gets the common elements between 2 lists
inter([], _, []).
inter([H1|T1], L2, [H1|Res]) :-
    member(H1, L2),
    inter(T1, L2, Res).
inter([_|T1], L2, Res) :-
    inter(T1, L2, Res).
% Returns only a single output which contains all elements shared between 2 lists
% can be a list of lists
get_inter(L1, L2, X) :- inter(L1, L2, X),!.
% ########################################################
