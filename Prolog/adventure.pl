#!/usr/bin/env swipl
/* Doges&Cateons, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka. */

:- dynamic i_am_at/1, at/2, enemy_at/2, holding/1, value_HP/2.
:- retractall(i_am_at(_)), retractall(at(_, _)), retractall(enemy_at(_, _)), retractall(holding(_)), retractall(value_HP(_, _)).

i_am_at(entrance).

% Map of the Egyptian tomb
path(entrance, n, antechamber).
path(antechamber, n, altar_room).
path(antechamber, s, entrance).
path(antechamber, w, jar_room).
path(antechamber, e, attendant_room).
path(jar_room, e, antechamber).
path(jar_room, w, accolite_chamber_1).
path(attendant_room, w, antechamber).
path(attendant_room, n, corridor).
path(corridor, n, false_floor_room).
path(corridor, w, altar_room).
path(accolite_chamber_1, n, accolite_chamber_2).
path(accolite_chamber_2, s, accolite_chamber_1).
path(accolite_chamber_2, e, serket_chamber).
path(altar_room, s, antechamber).
path(altar_room, e, corridor).
path(false_floor_room, s, corridor).
path(false_floor_room, n, trap_corridor).
path(trap_corridor, s, false_floor_room).
path(trap_corridor, n, treasure_room).
path(serket_chamber, w, accolite_chamber_2).
path(serket_chamber, n, guardian).
path(guardian, s, serket_chamber).
path(guardian, n, sarcophagus).
path(treasure_room, s, trap_corridor).
path(treasure_room, n, hidden_exit).
path(sarcophagus, s, guardian).
path(sarcophagus, e, hidden_exit).
path(hidden_exit, n, sarcophagus).
path(hidden_exit, s, treasure_room).

enemy_at(skele_cat, attendant_room).

value_HP(skele_cat, 1).
value_HP(you, 6).

defense(skele_cat, 10).
defense(you, 13).

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        not(enemy_at(_, Here)),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(Direction) :-
        i_am_at(Here),
        enemy_at(_, Here),
        path(Here, Direction, _),
        write('You cannot exit room, when is monster in it.').

go(_) :-
        write('You can''t go that way.').
/* This rule tells how to look about you. */
look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        notice_enemies_at(Place),
        notice_holding_objects. 


/* These rules are for combat. */
alive(Enemy) :-
        value_HP(Enemy, EnemyHP),
        EnemyHP > 0.

attack(Enemy) :-
        i_am_at(Place),
        enemy_at(Enemy, Place),
        alive(Enemy),
        random_between(1, 20, MyRoll),
        random_between(1, 20, EnemyRoll),
        (alive(Enemy) -> hit(you, Enemy, MyRoll) ; true),
        (alive(Enemy) -> hit(Enemy, you, EnemyRoll) ; true),
        !.
attack(Enemy) :-
        write('You cannot attack '), write(Enemy), write(' in this place.'), nl.

hit(Attacker, Defender, Roll) :-
        defense(Defender, Strength),
        (Roll >= Strength ->
                value_HP(Defender, HP),
                plus(NewHP, 1, HP),
                retract(value_HP(Defender, HP)),
                assert(value_HP(Defender, NewHP)),
                write(Attacker), write(' attacks '), write(Defender), write(' ('), write(Roll), write('>='), write(Strength), write('). He has now '), write(NewHP), write(' HP.'), nl,
                (NewHP == 0 -> write(Defender), write(' died.'), nl ; true)
        ;
                write(Attacker), write(' failed to attack '), write(Defender), write(' ('), write(Roll), write('<'), write(Strength), write(').'), nl).
flee(Direction) :-
        i_am_at(Here),
        enemy_at(Enemy, Here),
        go(Direction),
        !,
        path(Here, Direction, There),
        value_HP(you, HP),
        plus(NewHP, 1, HP),
        retract(value_HP(you, HP)),
        assert(value_HP(you, NewHP)),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        write(Enemy), write(' attacks you when you leave. You has now '), write(NewHP), write(' HP.'), nl,
        !, look.

flee(_) :-
        write('You can''t go that way.').

/* These rules are for noticing things. */
notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_) :-
        write('There is a nothing here.'), nl.


notice_enemies_at(Place) :-
        enemy_at(Enemy, Place),
        alive(Enemy),
        write('There is a '), write(Enemy), write(' here. Time to fight!'), nl,
        fail.

notice_enemies_at(_).

notice_holding_objects() :-
        holding(X),
        write('You have '), write(X), write(' in the inventory.'), nl,
        fail.


/* This rule tells how to die. */

die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on circumstances, a room may have more than one description. */

describe(entrance) :- write('Stoisz w tunelu prowadzącym do grobowca, przed tobą znajdują się uchylone wrota.'), nl.
describe(attendant_room) :- write('You are in simple room.'), nl.
