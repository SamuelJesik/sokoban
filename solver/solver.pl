:- consult('facts.pl').
:- use_module(state).
:- use_module(actions).
:- use_module(utils).

% BFS algoritmus na hľadanie riešenia
solve_bfs :-
    initial_state(Init),
    perform_bfs([[Init]], [], Solution),
    print_solution(Solution).

% BFS implementácia
perform_bfs([[State|Path]|_], _, [State|Path]) :-
    goal(State), !.

perform_bfs([[State|Path]|OtherPaths], Visited, Solution) :-
    is_valid_state(State, Visited),
    generate_next_states(State, Path, NextStates),
    append(OtherPaths, NextStates, NewFrontier),
    perform_bfs(NewFrontier, [State|Visited], Solution).

perform_bfs([[_|_]|OtherPaths], Visited, Solution) :-  
    perform_bfs(OtherPaths, Visited, Solution).

perform_bfs([], _, _) :-
    writeln('Žiadne riešenie nenájdené.'),
    fail.

% Overenie, či je stav validný
is_valid_state(State, Visited) :-
    \+ member(State, Visited),
    \+ deadlock(State).

% Generovanie nasledujúcich stavov
generate_next_states(State, Path, NextStates) :-
    findall([NextState, State|Path],
        (   move(State, _, NextState),
            \+ member(NextState, [State|Path])
        ),
        NextStates).
