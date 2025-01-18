:- module(actions, [move/3, action/3]).

action(up,    0, -1).
action(down,  0,  1).
action(left, -1,  0).
action(right, 1,  0).

move(State, Action, NewState) :-
    action(Action, DX, DY),
    calculate_new_position(State, Action, DX, DY, NewState),
    format('move(~w).~n', [Action]).

calculate_new_position(state(player(X, Y), Boxes), Action, DX, DY, state(player(NewX, NewY), NewBoxes)) :-
    NewX is X + DX,
    NewY is Y + DY,
    (
        ( \+ member(box(NewX, NewY), Boxes),
          \+ wall(NewX, NewY),
          NewBoxes = Boxes,
          debug_move(Action, 'Pohyb bez tlačenia', NewX, NewY)
        );
        ( member(box(NewX, NewY), Boxes),
          NewBoxX is NewX + DX,
          NewBoxY is NewY + DY,
          \+ wall(NewBoxX, NewBoxY),
          \+ member(box(NewBoxX, NewBoxY), Boxes),
          select(box(NewX, NewY), Boxes, box(NewBoxX, NewBoxY), NewBoxes),
          debug_move(Action, 'Pohyb s tlačením', NewX, NewY, NewBoxX, NewBoxY)
        )
    ).

debug_move(Action, Message, PlayerX, PlayerY) :-
    format('~w: ~w~n', [Message, Action]),
    format('Hráč sa presúva na: (~w, ~w)~n', [PlayerX, PlayerY]).

debug_move(Action, Message, PlayerX, PlayerY, BoxX, BoxY) :-
    format('~w: ~w~n', [Message, Action]),
    format('Hráč sa presúva na: (~w, ~w)~n', [PlayerX, PlayerY]),
    format('Krabica sa presúva na: (~w, ~w)~n', [BoxX, BoxY]).
