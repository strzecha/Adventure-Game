:- dynamic i_am_at/1, at/2, holding/1, is_discovered/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(is_discovered(_)), retractall(holding(_)).

/* Facts about the world */

i_am_at(someplace).

path(someplace, n, jungle).
path(jungle, s, someplace).
path(someplace, e, beach).
path(beach, w, someplace).

at(thing, someplace).
at(map, someplace).
at(tree, someplace).
at(flaming_torch, someplace).

at(native, jungle).

exchange(native, phone, ax).

is_dark(jungle).

is_pickable(thing).
is_pickable(flaming_torch).
is_pickable(map).

holding(notebook).
holding(phone).

is_reusable(ax).

usable(ax, tree, wood).

is_discovered(someplace).

/* These rules describe how to pick up an object */

take(Object) :-
        i_am_at(Location),
        at(Object, Location),
        is_pickable(Object),
        retract(at(Object, Location)),
        assert(holding(Object)),
        write('You picked up '), write(Object),
        !, nl.

take(Object) :-
        i_am_at(Location),
        at(Object, Location),
        write('You can''t pick it up.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.

/* These rules describe how to use objects */

use_up(Object) :-
        is_reusable(Object), !.

use_up(Object) :-
        holding(Object),
        retract(holding(Object)), !.

use_up(Object) :-
        i_am_at(Location),
        at(Object, Location),
        retract(at(Object, Location)).

available(Object) :-
        holding(Object), !.

available(Object) :-
        i_am_at(Location),
        at(Object, Location).

use(Object1, Object2) :-
        available(Object1),
        available(Object2),
        usable(Object1, Object2, Result),
        use_up(Object1),
        use_up(Object2),
        assert(holding(Result)),
        write('You got '), write(Result),
        nl, !.

use(Object1, Object2) :-
        available(Object1),
        write('You don''t have '), write(Object2), 
        nl, !.

use(Object1, _) :-
        write('You don''t have '), write(Object1),
        nl.

/* These rules describe how to put down an object */

drop(Object) :-
        holding(Object),
        i_am_at(Location),
        retract(holding(Object)),
        assert(at(Object, Location)),
        write('You dropped '), write(Object),
        !, nl.

drop(Object) :-
        write('You aren''t holding '), write(Object),
        nl.

/* These rules describe how to show your inventory */

inventory :-
        holding(Object),
        write(Object), nl,
        fail.

inventory.

/* These rules describe how to examine objects */

examine(Object) :-
        available(Object),
        describe(Object),
        !.

examine(Object) :-
        available(Object),
        write('You don''t know anything about '), write(Object),
        nl, !.

examine(Object) :-
        write('There is no '), write(Object),
        nl.

/* These rules describe how to interact with NPCs */

talk(NPC) :-
        i_am_at(Location),
        at(NPC, Location),
        speak(NPC),
        nl, !.

talk(NPC) :-
        i_am_at(Location),
        at(NPC, Location),
        write('You can''t do this'),
        nl, !.

talk(NPC) :-
        write('There is no '), write(NPC), 
        nl.

give(Object, NPC) :-
        i_am_at(Location),
        at(NPC, Location),
        exchange(NPC, Object, Gift),
        holding(Object),
        retract(holding(Object)),
        assert(holding(Gift)),
        write('You got '), write(Gift),
        nl, !.

give(Object, NPC) :-
        i_am_at(Location),
        at(NPC, Location),
        holding(Object),
        write('He don''t want it'),
        nl, !.

give(Object, NPC) :-
        i_am_at(Location),
        at(NPC, Location),
        write('You don''t have '), write(Object),
        nl, !.

give(_, NPC) :-
        write('There is no '), write(NPC),
        nl.


/* These rules define the direction letters as calls to go/1 */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction and discover new location */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, Location),
        retract(i_am_at(Here)),
        assert(i_am_at(Location)),
        discover(Location),
        !, look.

go(_) :-
        write('You can''t go that way.').

discover(Location) :-
        is_discovered(Location), !.

discover(Location) :-
        holding(map),
        assert(is_discovered(Location)), !.

discover(_).


/* This rule set up a loop to mention all the objects in location */

notice_objects_at(Location) :-
        at(Object, Location),
        write(Object), write(', '),
        fail.

/* This rule tells how to look about you */

print_objects(Location) :-
        i_am_at(Location),
        write('There are: '),
        notice_objects_at(Location),
        nl, !.

print_objects(_).

look :-
        i_am_at(Location),
        is_dark(Location),
        holding(flaming_torch),
        describe(Location),
        nl,
        print_objects(Location),
        nl, !.

look :-
        i_am_at(Location),
        is_dark(Location),
        write('You see darkness only. You need some light.'),
        nl, !.

look :-
        i_am_at(Location),
        describe(Location),
        nl,
        print_objects(Location),
        nl.


/* These rules describe how to check where you can go */

print(Location) :-
        is_discovered(Location),
        write(Location), !.

print(_) :-
        write('undiscovered').

look_around :-
        i_am_at(Location),
        path(Location, Direction, Somewhere),
        write(Direction), write(' -> '), print(Somewhere),
        nl, fail.

look_around.

/* This rule tells how to die */

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


/* This rule just writes out game instructions */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.           -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('use(Object, Object).     -- to use the objects together.'), nl,
        write('inventory.               -- to see the objects you are holding.'), nl,
        write('look.                    -- to inspect current locations.'), nl,
        write('look_around.             -- to see where you can go.'), nl,
        write('talk(NPC).               -- to talk with NPC.'), nl,
        write('give(Object, NPC).       -- to give an object to NPC'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are */

start :-
        instructions,
        look.


/* These rules describe the various locations */

describe(someplace) :- write('You are someplace.'), nl.
describe(jungle) :- write('You are in the jungle.'), nl.
describe(beach) :- write('You are on the beach.'), nl.

/* These rules describe the various objects */

describe(phone) :- write('Works, but no signal.'), nl.
describe(ax) :- write('Perfect for cutting trees.'), nl.

/* These rules describe the various NPCs */

/* These rules describe the dialogs with NPCs */

speak(native) :- write('Do you want an ax? I will give it to you, if you have something glowing.').

