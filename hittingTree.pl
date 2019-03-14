:- [diagnosis].


call_prob1(SD, COMP, OBS, HS, CS, OUT) :-
    % HS is empty (at start) list of conflict sets
    diagnosis(SD, COMP, OBS),
    tp(SD, COMP, OBS, [_|OUT], CS).
    [OUT|CS].
