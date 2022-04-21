#!/usr/bin/env swipl
/* Doges&Cateons, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka. */

:- dynamic i_am_at/1, at/2, enemy_at/2, holding/1, health/2.
:- discontiguous health/2, defense/2, enemy_at/2, damage/2.
:- retractall(i_am_at(_)), retractall(at(_, _)), retractall(enemy_at(_, _)), retractall(holding(_)), retractall(health(_, _)).

i_am_at(entrance).

/* Map of the Egyptian tomb */
% Rooms
path(entrance, n, antechamber).
path(antechamber, n, altar_room).
path(antechamber, s, entrance).
path(antechamber, w, jar_room).
path(antechamber, e, attendant_room).
path(jar_room, e, antechamber).
path(jar_room, w, acolyte_chamber_1).
path(attendant_room, w, antechamber).
path(attendant_room, n, corridor).
path(corridor, n, false_floor_room).
path(corridor, w, altar_room).
path(acolyte_chamber_1, n, acolyte_chamber_2).
path(acolyte_chamber_2, s, acolyte_chamber_1).
path(acolyte_chamber_2, e, serket_chamber).
path(altar_room, s, antechamber).
path(altar_room, e, corridor).
path(false_floor_room, s, corridor).
path(false_floor_room, n, trap_corridor).
path(trap_corridor, s, false_floor_room).
path(trap_corridor, n, treasure_room).
path(serket_chamber, w, acolyte_chamber_2).
path(serket_chamber, n, guardian).
path(guardian, s, serket_chamber).
path(guardian, n, sarcophagus).
path(treasure_room, s, trap_corridor).
path(treasure_room, n, hidden_exit).
path(sarcophagus, s, guardian).
path(sarcophagus, e, hidden_exit).
path(hidden_exit, n, sarcophagus).
path(hidden_exit, s, treasure_room).

% Door
door_between(acolyte_chamber_2, normal_door, serket_chamber).
door_between(serket_chamber, normal_door, acolyte_chamber_2).
door_between(acolyte_chamber_2, moonlight_door, acolyte_chamber_1).
door_between(acolyte_chamber_1, moonlight_door, acolyte_chamber_2).

door_closed(normal_door).
door_closed(moonlight_door).


/* Doge (player) stats */
health(you, 6).
defense(you, 12).
damage(you, 4).


/* Enemies. */
% Skeleton cat
enemy_at(skele_cat_1, attendant_room).
health(skele_cat_1, 3).
defense(skele_cat_1, 12).
damage(skele_cat_1, 2).

% Catmint guardian
enemy_at(catmint_guardian, guardian).
health(catmint_guardian, 12).
defense(catmint_guardian, 9).
damage(skele_cat_1, 6).

% Fallen cat
enemy_at(fallen_cat, sarcophagus).
health(fallen_cat, 6).
defense(fallen_cat, 12).
damage(skele_cat_1, 4).

/* Keys and objects */
% Key opening the door between acolyte_chamber_2 and serket_chamber, lying in attendant_room
at(key, attendant_room).

% Torch that needs to be carried out to open the door between acolyte_chamber_1 and acolyte_chamber_2, hanging in acolyte_chamber_1
at(torch, acolyte_chamber_1).


/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        alive(you),

        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        alive(you),

        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        alive(you),

        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */
drop(X) :-
        holding(X),
        i_am_at(Place),
        alive(you),

        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        alive(you),

        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* Rules to open door */
open_door(normal_door) :-
        holding(key),
        retract(door_closed(normal_door)),
        write("You unlocked the door.").

open_door(moonlight_door) :-
        door_closed(moonlight_door),
        not(holding(torch)),
        not(at(torch, acolyte_chamber_1)),
        not(at(torch, acolyte_chamber_2)),
        retract(door_closed(moonlight_door)),
        write("You unlocked the moonlight door.").

open_door(DoorName) :-
        write("The "), write(DoorName), write(" is locked."), nl,
        !, look.


/* Rules to go through the door */
go_through_door(DoorName) :-
        door_closed(DoorName),
        write("Trying to open door..."), nl.
        open_door(DoorName).


go_through_door(DoorName) :-
        not(door_closed(DoorName)),
        write("The door is opened and you went through."), nl.


/* Rules to check door */
check_door(Here, There) :-
        door_between(Here, DoorName, There),
        go_through_door(DoorName).

check_door(Here, There) :-
        not(door_between(Here, _, There)).


/* This rule tells how to move in a given direction. */
go(Direction) :-
        i_am_at(Here),
        room_cleared(Here),
        path(Here, Direction, There),
        alive(you),
        check_door(Here, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(Direction) :-
        i_am_at(Here),
        enemy_at(_, Here),
        path(Here, Direction, _),
        alive(you),

        write('You cannot exit room, when is monster in it.'), nl, !.

go(_) :-
        write('You can''t go that way.').

/* This rule tells how to look about you. */
look :-
        i_am_at(Place),
        alive(you),

        describe(Place),
        findall(Direction, path(acolyte_chamber_1, Direction, _), Directions),
        write('Possible exits: '), write(Directions), nl,
        notice_enemies_at(Place).

notice_enemies_at(Place) :-
        enemy_at(Enemy, Place),
        alive(Enemy),

        write('There is a '), write(Enemy), write(' here. Time to fight!'), nl, !.

notice_enemies_at(_).

/* These rules are for combat. */
alive(you) :-
        health(you, HP),
        HP =< 0,
        write('You are dead. Please enter the "halt." command1.'), fail, !.

alive(Enemy) :-
        health(Enemy, EnemyHP),
        EnemyHP > 0, !.

attack(Enemy) :-
        i_am_at(Place),
        enemy_at(Enemy, Place),
        alive(you),

        hit(you, Enemy),
        ((alive(Enemy)) -> hit(Enemy, you) ; true), !.

attack(Enemy) :-
        alive(you),

        write('You cannot attack '), write(Enemy), write(' in this place.'), nl, !.

hit(Attacker, Defender) :-
        defense(Defender, Strength),
        random_between(1, 20, Roll),
        (Roll >= Strength ->
                damage(Attacker, MaxDamage),
                random_between(1, MaxDamage, Damage),
                write(Attacker), write(' attacks '), write(Defender), write(' ('), write(Roll), write('>='),
                write(Strength), write(') ['), write(Damage), write(' dmg]. '),
                harm(Defender, Damage)
        ;
                write(Attacker), write(' failed to attack '), write(Defender), write(' ('), write(Roll), write('<'), write(Strength), write(').'), nl).

harm(Character, Damage) :-
        health(Character, HP),

        plus(NewHP, Damage, HP),
        retract(health(Character, HP)),
        assert(health(Character, NewHP)),
        (NewHP =< 0 -> write(Character), write(' died.'), nl ; write('Remaining HP: '), write(NewHP), nl).

flee(Direction) :-
        i_am_at(Here),
        enemy_at(Enemy, Here),
        path(Here, Direction, There),
        alive(you),

        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        write(Enemy), write(' attacks you when you leave. '),
        harm(you, 1),
        !, look.

flee(_) :-
        write('You can\'t go that way.'), !.


/* These rules are for searching rooms. */
search :-
        i_am_at(Place),
        at(X, Place),
        room_cleared(Place),
        alive(you),

        write('There is a '), write(X), write(' here.'), nl, !.

search :-
        i_am_at(Place),
        enemy_at(Enemy, Place),
        alive(Enemy),
        alive(you),

        write('You can\'t search the room when is '), write(Enemy), write(' there.'), nl, !.

search :-
        i_am_at(Place),
        not(at(_, Place)),
        alive(you),

        write('There is nothing here.'), nl, !.

room_cleared(Place) :-
        not(enemy_at(_, Place)) ; (enemy_at(Enemy, Place), not(alive(Enemy))).

% Inventory functions
i :- inventory.

inventory :-
        holding(X),
        alive(you),

        write('You have '), write(X), write(' in the inventory.'), nl.

inventory :-
        not(holding(_)),
        alive(you),

        write('You don\'t have anything in you inventory.'), nl.

/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

/* This rule just writes out game instructions. */
instructions :-
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('flee(Direction)    -- to flee from combat.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('search.            -- to search the room.'), nl,
        write('attack(Enemy).     -- to attack the enemy.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */
start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on circumstances, a room may have more than one description. */
describe(entrance) :- write('Stoisz w tunelu prowadzącym do grobowca, przed tobą znajdują się uchylone wrota.'), nl, !.
describe(attendant_room) :- write('You are in simple room.'), nl, !.
describe(_) :- write('This room is not implemented'), nl.
