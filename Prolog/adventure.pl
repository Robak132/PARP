/* Pieseły i Locheły, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka. */

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(holding(_)), retractall(i_am_at(_)), retractall(alive(_)).

i_am_at(entance).

% Map of the Egyptian tomb
path(entance, n, antechamber).
path(antechamber, n, altar_room).
path(antechamber, s, entance).
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
path(trap_corridor, n, tresure_room).
path(serket_chamber, w, accolite_chamber_2).
path(serket_chamber, n, guardian).
path(guardian, s, serket_chamber).
path(guardian, n, sarcophagus).
path(tresure_room, s, trap_corridor).
path(tresure_room, n, hidden_exit).
path(sarcophagus, s, guardian).
path(sarcophagus, e, hidden_exit).
path(hidden_exit, n, sarcophagus).
path(hidden_exit, s, tresure_room).

% at(bowl, kitchen).
% at(cornflakes, kitchen).
% at(milk, kitchen).

% crafting_recipe(bowl, milk, bowl_with_milk).
% crafting_recipe(bowl_with_milk, cornflakes, monster_meal).
% crafting_recipe(bowl, cornflakes, bowl_with_cornflakes).
% crafting_recipe(bowl_with_cornflakes, milk, breakfast).

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

% craft(X, Y) :-
%         holding(X),
%         holding(Y),
%         crafting_recipe(X, Y, A),
%         !,
%         assert(holding(A)),
%         retract(holding(X)),
%         retract(holding(Y)),
%         write('You created '), write(A),
%         look.

% craft(_, _) :-
%         write('Something went wrong!'),
%         nl.

/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl,
        notice_holding_objects.


/* These rules set up a loop to mention all the objects in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

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

describe(entance) :- write('Stoisz w tunelu prowadzącym do grobowca, przed tobą znajdują się uchylone wrota.'), nl.

