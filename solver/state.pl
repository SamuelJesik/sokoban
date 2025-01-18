:- module(state, [initial_state/1, goal/1]).

initial_state(State) :-
    initialize_player(Player),
    initialize_boxes(Boxes),
    State = state(Player, Boxes),
    debug_initial_state(State).

initialize_player(player(Px, Py)) :-
    initial_player(Px, Py).

initialize_boxes(Boxes) :-
    findall(box(Bx, By), initial_box(Bx, By), Boxes).

debug_initial_state(State) :-
    format('Počiatočný stav: ~w~n', [State]).

goal(State) :-
    boxes_on_storages(State),
    debug_goal_reached(State).

boxes_on_storages(state(_, Boxes)) :-
    forall(member(box(Bx, By), Boxes), storage(Bx, By)).

debug_goal_reached(state(_, Boxes)) :-
    format('Cieľ dosiahnutý! Krabice: ~w~n', [Boxes]).
