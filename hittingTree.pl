:- [diagnosis].
:- style_check(-singleton). % Hides singleton variable warning message


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

pair_firsts([X|Xs], [Y|Ys], [X,Y]).

generate_pairs([X|Xs], [H,Y|Ys], [X,Y]).

generate_all_pairs(X, Y, Out):-
  pair_firsts(X,Y,Firsts),
  append([], Firsts, Out),
  generate_pairs(X, Y, Out).


%problem1_solver(SD, COMP, OBS, HS, CS, OUT, OUT).
problem1_solver(SD, COMP, OBS, HS, CS, OUT, OUT):-
  problem1(SD, COMP, OBS),
  tp(SD, COMP, OBS, [], CS).

% generate_pairs(X, Y, Out):-
%   (is_list(X), is_list(Y)) ->
%   get_first_element(X,A),
%   get_first_element(Y,B),
%   concatenate([A],[B], C),
%   get_tail(Y,TY),
%   write("reached listY "),
%   generate_pairs(X, TY, [C|Out]);
%   (is_list(X), atom(Y)) ->
%   write("reached atomY "),
%   write("nolist "), Out.

get_tail([X|Xs], Xs).


  % listsplit(Y, HY, TY), generate_pairs(X, TY, Out);
  % write("atom").

% pair_firsts(X, Y, Out):-
%   get_first_element(X,A),
%   get_first_element(Y,B),
%   concatenate([A],[B], Out).
%
% concatenate(A, B, Out):-
%   A\==B -> append([A],[B],Out);
%   Out = A.
%
% get_first_element([X|Xs], Out):-
%   is_list(X) ->  get_first_element(X, Out);
%   Out = X.


  % write("reached_concatenate"), atom(X), atom(Y)->  append(X, Y, Out);
  % write("not equal"), append(X, Y, Out).

%(X \== Y) ,
%[[X1, X2],[X1, A2, O1]]

% [X1, X2],[X1, A2, O1], Out

%[X1, X2],[X3, A2, O1], Out

  %write(print_set(X)),print_set(XS).

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
