/* Adventure, by Strzecha. */

:- dynamic i_am_at/1, at/2, holding/1, is_discovered/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(is_discovered(_)), retractall(holding(_)).

i_am_at(someplace).

path(someplace, n, jungle).
path(jungle, s, someplace).
path(someplace, e, beach).
path(beach, w, someplace).

at(thing, someplace).
at(tree, someplace).
at(flaming_torch, someplace).

at(native, jungle).

speak(native) :-
        write('Do you want an ax? I will give it to you, if you have something glowing.').

exchange(native, phone, ax).

is_dark(beach).

is_pickable(thing).
is_pickable(flaming_torch).

holding(notebook).
holding(phone).

is_reusable(ax).

usable(ax, tree, wood).

is_discovered(someplace).

/* These rules describe how to pick up an object. */

take(X) :-
        i_am_at(Place),
        at(X, Place),
        is_pickable(X),
        retract(at(X, Place)),
        assert(holding(X)),
        write('You picked up '), write(X),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        write('You can''t pick it up.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.

use_up(Object) :-
        is_reusable(Object), !.

use_up(Object) :-
        holding(Object),
        retract(holding(Object)), !.

use_up(Object) :-
        i_am_at(Place),
        at(Object, Place),
        retract(at(Object, Place)), !.

available(Object) :-
        holding(Object), !.

available(Object) :-
        i_am_at(Place),
        at(Object, Place), !.

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
        nl, !.

/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('You dropped '), write(X),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.

inventory :-
        holding(X),
        write(X), nl,
        fail.

inventory.

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
        nl, !.
        
/**/

talk(NPC) :-
        i_am_at(Place),
        at(NPC, Place),
        speak(NPC),
        nl, !.

talk(NPC) :-
        i_am_at(Place),
        at(NPC, Place),
        write('You can''t do this'),
        nl, !.

talk(NPC) :-
        write('There is no '), write(NPC), 
        nl, !.

give(Object, NPC) :-
        i_am_at(Place),
        at(NPC, Place),
        exchange(NPC, Object, Gift),
        holding(Object),
        retract(holding(Object)),
        assert(holding(Gift)),
        write('You got '), write(Gift),
        nl, !.

give(Object, NPC) :-
        i_am_at(Place),
        at(NPC, Place),
        holding(Object),
        write('He don''t want it'),
        nl, !.

give(Object, NPC) :-
        i_am_at(Place),
        at(NPC, Place),
        write('You don''t have '), write(Object),
        nl, !.

give(_, NPC) :-
        write('There is no '), write(NPC),
        nl, !.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, Place),
        retract(i_am_at(Here)),
        assert(i_am_at(Place)),
        discover(Place),
        !, look.

go(_) :-
        write('You can''t go that way.').

discover(Place) :-
        is_discovered(Place).

discover(Place) :-
        assert(is_discovered(Place)).


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        is_dark(Place),
        holding(flaming_torch),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl, !.

look :-
        i_am_at(Place),
        is_dark(Place),
        write('You see darkness only. You need some light.'),
        nl, !.

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl, !.

print(Place) :-
        is_discovered(Place),
        write(Place), !.

print(_) :-
        write('undiscovered').

look_around :-
        i_am_at(Place),
        path(Place, Direction, Somewhere),
        write(Direction), write(' -> '), print(Somewhere),
        nl, fail.

look_around.

/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


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


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(someplace) :- write('You are someplace.'), nl.
describe(jungle) :- write('You are in the jungle.'), nl.
describe(beach) :- write('You are on the beach.'), nl.

describe(phone) :- write('Works, but no signal.'), nl.
describe(ax) :- write('Perfect for cutting trees.'), nl.

