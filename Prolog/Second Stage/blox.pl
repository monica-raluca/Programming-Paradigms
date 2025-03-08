:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').


%% TODO
% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4
empty_state(state([], [], [], [], [])).

%%%%%%
% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

%% TODO
% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.
set_tile(state(Tiles, Fragiles, Targets, Switches, Block), Pos, state([Pos|Tiles], Fragiles, Targets, Switches, Block)).

%% TODO
% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.
set_blank(S, _, S).

%% TODO
% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).
set_target(state(Tiles, Fragiles, Targets, Switches, Block), Pos, state(Tiles, Fragiles, [Pos|Targets], Switches, Block)).

%% TODO
% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.
set_fragile(state(Tiles, Fragiles, Targets, Switches, Block), Pos, state(Tiles, [Pos|Fragiles], Targets, Switches, Block)).


%% TODO
% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.
set_block_initial(state(Tiles, Fragiles, Targets, Switches, _), Pos, state([Pos|Tiles], Fragiles, Targets, Switches, block(Pos, upright))).

%% TODO
% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)]
get_b_pos(state(_, _, _, _, block(Pos, upright)), Pos).
get_b_pos(state(_, _, _, _, block(Pos1, Pos2, horizontalOX)), [Pos1, Pos2]).
get_b_pos(state(_, _, _, _, block(Pos1, Pos2, horizontalOY)), [Pos1, Pos2]).



%% TODO
% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.
get_bounds(state(Tiles, Fragiles, Targets, Switches, _), Xmin, Xmax, Ymin, Ymax) :-
    findall(X, (member((X, _), Tiles); member((X, _), Fragiles); member((X, _), Targets); member((X, _), Switches)), Xs),
    findall(Y, (member((_, Y), Tiles); member((_, Y), Fragiles); member((_, Y), Targets); member((X, _), Switches)), Ys),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax).

%% TODO
% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.
get_cell(state(Tiles, _, _, _, _), Pos, tile) :- member(Pos, Tiles).
get_cell(state(_, Fragiles, _, _, _), Pos, fragile) :- member(Pos, Fragiles).
get_cell(state(_, _, Targets, _, _), Pos, target) :- member(Pos, Targets).
get_cell(state(_, _, _, Switches, _), Pos, oswitch) :- member(switch(Pos, oswitch, _, _, _), Switches).
get_cell(state(_, _, _, Switches, _), Pos, xswitch) :- member(switch(Pos, xswitch, _, _, _), Switches).
get_cell(state(_, _, _, Switches, _), Pos, tile) :- 
    member(switch(_, _, _, Positions, off), Switches),
    \+ member(Pos, Positions).
get_cell(state(_, _, _, Switches, _), Pos, tile) :- 
    member(switch(_, _, _, Positions, on), Switches),
    member(Pos, Positions).

%% TODO
% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).
valid_tile(Pos, Tiles, Fragiles, Targets, Switches) :-
    member(Pos, Tiles);
    member(Pos, Fragiles);
    member(Pos, Targets);
    member(switch(Pos, _, _, _, _), Switches);
    (member(switch(_, _, _, Positions, on), Switches), member(Pos, Positions)).

horizontal_condition(horizontalOX, Move) :- Move \= l, Move \= r.
horizontal_condition(horizontalOY, Move) :- Move \= u, Move \= d.

move(state(Tiles, Fragiles, Targets, Switches, block(Pos1, Pos2, Horizontal)), Move, state(Tiles, Fragiles, Targets, UpdatedSwitches, block(Pos1New, Pos2New, Horizontal))) :-
    horizontal_condition(Horizontal, Move),
    neighbor(Pos1, Move, Pos1New),
    neighbor(Pos2, Move, Pos2New),
    valid_tile(Pos1New, Tiles, Fragiles, Targets, Switches), !,
    valid_tile(Pos2New, Tiles, Fragiles, Targets, Switches),
    update_switches(Switches, Pos1New, Pos2New, UpdatedSwitches, block(_, Horizontal)).

move(state(Tiles, Fragiles, Targets, Switches, block(Pos1, _, horizontalOX)), Move, state(Tiles, Fragiles, Targets, UpdatedSwitches, block(Pos1New, upright))) :-
    Move \= u, Move \= d,
    (Move \= r, neighbor(Pos1, Move, Pos1New); Move \= l, neighbor2(Pos1, Move, Pos1New)),
    \+member(Pos1New, Fragiles),
    update_switches(Switches, Pos1New, none, UpdatedSwitches, block(_, upright)).

move(state(Tiles, Fragiles, Targets, Switches, block(Pos1, _, horizontalOY)), Move, state(Tiles, Fragiles, Targets, UpdatedSwitches, block(Pos1New, upright))) :-
    Move \= l, Move \= r,
    (Move \= d, neighbor(Pos1, Move, Pos1New); Move \= u, neighbor2(Pos1, Move, Pos1New)),
    \+member(Pos1New, Fragiles),
    update_switches(Switches, Pos1New, none, UpdatedSwitches, block(_, upright)).

move(state(Tiles, Fragiles, Targets, Switches, block(Pos, upright)), Move, state(Tiles, Fragiles, Targets, UpdatedSwitches, block(Pos1New, Pos2New, horizontalOX))) :-
    Move \= u, Move \= d,
    (Move \= l, neighbor(Pos, Move, Pos1New), neighbor2(Pos, Move, Pos2New); Move \= r, neighbor2(Pos, Move, Pos1New), neighbor(Pos, Move, Pos2New)),
    valid_tile(Pos1New, Tiles, Fragiles, Targets, Switches), !,
    valid_tile(Pos2New, Tiles, Fragiles, Targets, Switches),
    update_switches(Switches, Pos1New, Pos2New, UpdatedSwitches, block(_, horizontalOX)).

move(state(Tiles, Fragiles, Targets, Switches, block(Pos, upright)), Move, state(Tiles, Fragiles, Targets, UpdatedSwitches, block(Pos1New, Pos2New, horizontalOY))) :-
    Move \= l, Move \= r,
    (Move \= u, neighbor(Pos, Move, Pos1New), neighbor2(Pos, Move, Pos2New); Move \= d, neighbor2(Pos, Move, Pos1New), neighbor(Pos, Move, Pos2New)),
    valid_tile(Pos1New, Tiles, Fragiles, Targets, Switches), !,
    valid_tile(Pos2New, Tiles, Fragiles, Targets, Switches),
    update_switches(Switches, Pos1New, Pos2New, UpdatedSwitches, block(_, horizontalOY)).

update_switches(Switches, Pos1, Pos2, UpdatedSwitches, Block) :-
    maplist(update_switch(Pos1, Pos2, Block), Switches, UpdatedSwitches).

update_switch(Pos1, Pos2, block(_, Orientation), switch(Pos, Type, Func, Positions, ActivationState), switch(Pos, Type, Func, Positions, NewActivationState)) :-
    (member(Pos, [Pos1, Pos2]) ->
        ((Type = oswitch;
          (Type = xswitch, Orientation = upright)) ->
            (Func = uponly, NewActivationState = on, !;
             Func = dnonly, NewActivationState = off, !;
             Func = switch, (ActivationState = on -> NewActivationState = off, !; NewActivationState = on))
        ;
            NewActivationState = ActivationState)
    ;
        NewActivationState = ActivationState).

%% TODO
% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).
is_final(state(_, _, Targets, _, block(Pos, upright))) :- member(Pos, Targets).

%%%%%%%%%% Etapa 2

%% TODO
% set_switch/6
% set_switch(+S, +Pos, +Switch, +Func, +Positions, -SNew)
% Leagă starea SNew la o stare cu aceleași informații ca și S, și în
% plus un switch la poziția Pos, cu parametrii dați.
%
% Switch: oswitch sau xswitch.
% Func: switch, uponly sau dnonly.
% Positions: pozițiile podului.
set_switch(state(Tiles, Fragiles, Targets, Switches, Block), Pos, Switch, Func, Positions, state(Tiles, Fragiles, Targets, [switch(Pos, Switch, Func, Positions, "off")|Switches], Block)).

%% TODO
% solve/2
% solve(+S, -Moves)
% Solve găsește o soluție pentru problema din starea S. Soluția este
% reprezentată ca secvența de mutări Moves.
%
% Pentru a fi soluție, mutările din Moves trebuie să ducă blocul în
% picioare pe poziția scop (target).
solve(S, Moves) :-
    search([[S, []]], [], Moves).

search([[State, Path]|_], _, Moves) :-
    is_final(State), !,
    reverse(Path, Moves).

search([[State, _]|Queue], Visited, Moves) :-
    member(State, Visited), !,
    search(Queue, Visited, Moves).

search([[State, Path]|Queue], Visited, Moves) :-
    findall(
        [NextState, [Move|Path]],
        (member(Move, [u, d, l, r]), move(State, Move, NextState), \+ member(NextState, Visited)),
        NextStates
    ),
    append(Queue, NextStates, NewQueue),
    search(NewQueue, [State|Visited], Moves).

