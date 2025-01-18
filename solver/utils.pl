:- module(utils, [print_solution/1, deadlock/1]).

print_solution(Solution) :-
    reverse(Solution, CorrectOrder),
    maplist(print_state, CorrectOrder).

print_state(State) :-
    writeln(State).

deadlock(state(_, Boxes)) :-
    member(box(Bx, By), Boxes),
    \+ storage(Bx, By),
    adjacent_walls(Bx, By),
    format('(~w, ~w)~n', [Bx, By]).

adjacent_walls(Bx, By) :-
    (wall(Bx, By - 1), wall(Bx - 1, By));
    (wall(Bx, By - 1), wall(Bx + 1, By));
    (wall(Bx, By + 1), wall(Bx - 1, By));
    (wall(Bx, By + 1), wall(Bx + 1, By)).
