:- [diagnosis, tp].
:- style_check(-singleton). % Hides singleton variable warning message
:- use_module(library(ordsets)).
% https://stackoverflow.com/questions/10563818/prolog-passing-a-function-as-a-variable-how-to-add-arguments/10564752

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
    problem2(SD, COMP, OBS),
    tp(SD, COMP, OBS, HS, CS),
    append(HS, CS, Z),
    call_problem2(SD, COMP, OBS, Z, CCS, [OUT|CS], O).

call_problem3(SD, COMP, OBS, HS, CS, OUT, OUT).
call_problem3(SD, COMP, OBS, HS, CS, OUT, O) :-
    problem3(SD, COMP, OBS),
    tp(SD, COMP, OBS, HS, CS),
    append(HS, CS, Z),
    call_problem3(SD, COMP, OBS, Z, CCS, [OUT|CS], O).

call_problem4(SD, COMP, OBS, HS, CS, OUT, OUT).
call_problem4(SD, COMP, OBS, HS, CS, OUT, O) :-
    fulladder(SD, COMP, OBS),
    tp(SD, COMP, OBS, HS, CS),
    append(HS, CS, Z),
    call_problem4(SD, COMP, OBS, Z, CCS, [OUT|CS], O).

% call_problem5(SD, COMP, OBS, HS, CS, OUT, OUT).
% call_problem5(SD, COMP, OBS, HS, CS, OUT, O) :-
%     problem5(SD, COMP, OBS),
%     tp(SD, COMP, OBS, HS, CS),
%     append(HS, CS, Z),
%     call_problem5(SD, COMP, OBS, Z, CCS, [OUT|CS], O).

% Gets the complete critical set of a diagnostic problem.
get_problem1(X) :-
    findall(OUTPUT, call_problem1(SD, COMPS, OBS, [], CS, [], OUTPUT), O),
    last(O, X).

get_problem2(X) :-
    findall(OUTPUT, call_problem2(SD, COMPS, OBS, [], CS, [], OUTPUT), O),
    last(O, X).

get_problem3(X) :-
    findall(OUTPUT, call_problem3(SD, COMPS, OBS, [], CS, [], OUTPUT), O),
    last(O, X).

get_problem4(X) :-
    findall(OUTPUT, call_problem4(SD, COMPS, OBS, [], CS, [], OUTPUT), O),
    last(O, X).

%get_problem5(X) :-
%    findall(OUTPUT, call_problem5(SD, COMPS, OBS, [], CS, [], OUTPUT), O),
%    last(O, X).
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

% ################## create set ##########################
% create_set() should create the complete tree of hitting sets

% ?- create_set([[X1, X2],[X1, A2, O1]], [], OUT).
% ?- create_set([a1, o1, a2], OUT).

create_set(HS, _, OUTPUT) :-
    % if only a single set input --> done.
    is_done(HS) -> OUTPUT = HS, !.
create_set([L|LS], BRANCH, OUTPUT) :-
    fail.


% #################### Other #############################

% checks if a list L is equal to flatten(L)
% if true - then we don't need to build a tree we have our
% minimal hitting set.
% ?- is_done([a1, o2, a2]). --> true
% ?- is_done([[x1, x2], [x1, a2, o1]]). --> fail
is_done(HS) :-
    flatten(HS, L),
    HS == L.

member3(X, [H|T]) :- X = H ; member3(X, T).

is_not_member(_, _, TEMP, TEMP).
is_not_member([L|LS], L1, TEMP, OUT) :-
    write("L = "), write(L),
    not(member3(L1, L)) ->
    append(TEMP, L, X);
    is_not_member(LS, L1, X, OUT).

% ################ Shortest list #########################
% returns the length of the shortest list .
% ?- minList([[x1, x2], [x1, a2, o1]], 9999, Best).
minList([], Best, Best).
minList([L|Ls], Best, Len1) :-
    length(L, N),
    ( N < Best ) -> minList(Ls, N, Len1);
    minList(Ls, Best, Len1).

% find a lists of length N
findshortList([L|_], N, L) :- length(L, N).
findshortList([L|Ls], N, List) :-
    findshortList(Ls, N, List).

% finds -all- the shortest sublists
findShortestList(IN, LL) :-
    minList(IN, 9999, Best),
    findall(L, findshortList(IN, Best, L), LL).
% ########################################################








% ######################################################

% finds the possible hitting sets for a diagnostics problem
solver(SD, COMP, OBS, HS, TREE, CS, TREE) :-
    not(tp(SD, COMP, OBS, HS, CS)).
solver(SD, COMP, OBS, HS, TREE, CS, OUT) :-
    tp(SD, COMP, OBS, HS, CS), % [x1, x2]
    member(X, CS),
    % write("> Branch: "), writeln([X|TREE]),
    solver(SD, COMP, OBS, [X|HS], [X|TREE], CSS, OUT).

% used to call the solver and find all solution
solve(X) :-
    problem3(SD, COMP, OBS),
    findall(OUT, solver(SD, COMP, OBS, [], [], CS, OUT), X).


% prunes the hitting set
% Removes duplicates, and supersets.
% prune(LS, LS, LS) :- !.
% prune(LS, Ls, R) :-
%
%         select(T, LS, [LH|LT]),
%         subset(LH, T)
%     ->
%         prune([LH|LT], [LH|LT], R)
%     ;
%         prune(LS, LS, R).

prune(_, Acc, Acc).
prune([L1|Ls], Acc, R) :-

        member(X, Ls),
        subset(X, L1)
    ->
        prune(Ls, [], R) % currently stops as soon as a single superset is found.
    ;
        prune(Ls, L1, R).




testprune(RR) :-
    L = [[a2, a1], [o1, a1], [o1], [a1, a2], [o1, a2]],
    length(L, N),
    % prune(L, [], R).
    findall(R, prune(L, [], R), RR).

temp(R, T, Out) :-
    Out = [R|[T]].









%
