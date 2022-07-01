:- dynamic i_am_at/1, at/2, holding/1, is_discovered/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(is_discovered(_)), retractall(holding(_)).

/* Facts about the world */

path(meadow1, e, forest1).
path(forest1, w, meadow1).
path(forest1, e, dark_jungle1).
path(dark_jungle1, w, forest1).
path(dark_jungle1, e, tunnel_entrance).
path(tunnel_entrance, w, dark_jungle1).
path(meadow1, s, forest2).
path(forest2, n, meadow1).
path(forest1, s, meadow2).
path(meadow2, n, forest1).
path(dark_jungle1, s, jungle1).
path(jungle1, n, dark_jungle1).
path(tunnel_entrance, s, tunnel1).
path(tunnel1, n, tunnel_entrance).
path(ancient_ruins, s, tunnel2).
path(tunnel2, n, ancient_ruins).
path(volcano_peak, s, mountain_path1).
path(mountain_path1, n, volcano_peak).
path(forest2, e, meadow2).
path(meadow2, w, forest2).
path(meadow2, e, jungle1).
path(jungle1, w, meadow2).
path(tunnel1, e, tunnel2).
path(tunnel2, w, tunnel1).
path(forest2, s, fields1).
path(fields1, n, forest2).
path(jungle1, s, jungle2).
path(jungle2, n, jungle1).
path(tunnel2, s, cave).
path(cave, s, tunnel2).
path(mountain_path1, s, mountain_path2).
path(mountain_path2, n, mountain_path1).
path(jungle2, e, dark_jungle2).
path(dark_jungle2, w, jungle2).
path(fields1, s, beach1).
path(beach1, n, fields1).
path(plane_wreck, s, fields2).
path(fields2, n, plane_wreck).
path(jungle2, s, crossroads).
path(crossroads, n, jungle2).
path(dark_jungle2, s, path1).
path(path1, n, dark_jungle2).
path(mountain_path2, s, path2).
path(path2, n, mountain_path2).
path(ocean1, e, beach1).
path(beach1, w, ocean1).
path(beach1, e, fields2).
path(fields2, w, beach1).
path(fields2, e, crossroads).
path(crossroads, w, fields2).
path(crossroads, e, path1).
path(path1, w, crossroads).
path(path1, e, path3).
path(path3, w, path1).
path(path3, e, path2).
path(path2, w, path3).
path(beach1, s, beach2).
path(beach2, n, beach1).
path(crossroads, s, village1).
path(village1, n, crossroads).
path(path1, s, well_square).
path(well_square, n, path1).
path(path3, s, village2).
path(village2, n, path3).
path(ocean2, e, beach2).
path(beach2, w, ocean2).
path(house1, e, village1).
path(village1, w, house1).
path(village1, e, well_square).
path(well_square, w, village1).
path(well_square, e, village2).
path(village2, w, well_square).
path(beach2, s, beach3).
path(beach3, n, beach2).
path(house1, s, pantry).
path(pantry, n, house1).
path(village1, s, house2).
path(house2, n, village1).
path(well_square, s, old_house).
path(old_house, n, well_square).
path(ocean3, e, beach3).
path(beach3, w, ocean3).
path(old_house, e, basement).
path(basement, w, old_house).

is_dark(tunnel_entrance).
is_dark(tunnel1).
is_dark(tunnel2).
is_dark(ancient_ruins).
is_dark(cave).
is_dark(dark_jungle1).
is_dark(dark_jungle2).
is_dark(basement).
is_dark(pantry).

at(totem, meadow1).
at(tree, forest1).
at(tree, forest1).
at(stick, forest1).
at(banana_tree, dark_jungle1).
at(lava_source, volcano_peak).
at(tree, forest2).
at(tree, forest2).
at(pond, meadow2).
at(rose, meadow2).
at(dandelion, meadow2).
at(bush, jungle1).
at(tree, jungle1).
at(note3, tunnel1).
at(iron_ore, tunnel2).
at(note2, mountain_path1).
at(note4, fields1).
at(notebook, plane_wreck).
at(bush, jungle2).
at(bush, dark_jungle2).
at(stick, dark_jungle2).
at(palm, beach1).
at(signpost, crossroads).
at(stone, path2).
at(palm, beach2).
at(palm, beach2).
at(stick, beach2).
at(note1, village1).
at(well, well_square).
at(string, village2).
at(note5, ocean3).
at(palm, beach3).
at(meat, pantry).
at(cloth, house2).
at(sheets, house2).
at(blast_furnace, old_house).
at(anvil, basement).

at(ancient_guard, ancient_ruins).
at(blacksmith, old_house).
at(monkey, jungle1).
at(native, house1).
at(old_native, cave).

is_pickable(stick).
is_pickable(rose).
is_pickable(dandelion).
is_pickable(note1).
is_pickable(note2).
is_pickable(note3).
is_pickable(note4).
is_pickable(note5).
is_pickable(notebook).
is_pickable(stone).
is_pickable(string).
is_pickable(meat).
is_pickable(cloth).
is_pickable(sheets).

exchange(native, phone, ax).
exchange(monkey, banana, rope).
exchange(native, rose, map).
exchange(ancient_guard, sword, mysterious_stone).
exchange(old_native, meat, banana).
exchange(old_native, dandelion, bucket).
exchange(blacksmith, fish, pickax).
exchange(blacksmith, raw_iron, sword).

usable(stone, banana_tree, banana).
usable(ax, tree, wood).
usable(ax, palm, mast).
usable(ax, bush, brushwood).
usable(sword, bush, brushwood).
usable(brushwood, cloth, torch).
usable(torch, lava_source, flaming_torch).
usable(pickax, iron_ore, raw_iron).
usable(stick, cloth, torch).
usable(stick, string, fishing_rod).
usable(raw_iron, blast_furnace, liquid_iron).
usable(liquid_iron, stick, sword_form).
usable(fishing_rod, pond, fish).
usable(sword_form, anvil, hot_sword).
usable(bucket, well, water_bucket).
usable(bucket, pond, water_bucket).
usable(hot_sword, water_bucket, sword).
usable(hot_sword, pond, sword).
usable(wood, wood, hardwood).
usable(hardwood, rope, deck).
usable(sheets, mast, sail).
usable(sail, deck, raft).
usable(torch, blast_furnace, flaming_torch).
usable(mysterious_stone, totem, people).

is_reusable(ax).
is_reusable(sword).
is_reusable(pickax).
is_reusable(fishing_rod).
is_reusable(pond).
is_reusable(well).
is_reusable(totem).
is_reusable(banana_tree).
is_reusable(lava_source).
is_reusable(blast_furnace).
is_reusable(anvil).

holding(phone).
i_am_at(fields2).

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
        assert(is_pickable(Result)),
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
        assert(is_pickable(Gift)),
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

/* These rules tells how to leave the island */
at_ocean :-
        i_am_at(ocean1); i_am_at(ocean2); i_am_at(ocean3).

has_notes :-
        holding(note1),
        holding(note2),
        holding(note3),
        holding(note4),
        holding(note5),
        holding(notebook).

leave :-
        at_ocean,
        holding(raft),
        has_notes,
        holding(people),
        write('You left the island. You rescued rest of the passengers and complete the history. You won!'), nl,
        finish,
        !.

leave :-
        at_ocean,
        holding(raft),
        has_notes,
        write('You left the island. You complete the history, but didn''t find rest of the passengers.'), nl,
        finish,
        !.

leave :-
        at_ocean,
        holding(raft),
        write('You left the island, but dont''t know anuthing.'), nl,
        finish,
        !.

leave :-
        at_ocean,
        write('You need something to '),
        !.

leave :-
        write('You aren''t at the ocean.').

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

describe(jungle1) :- write('You are in the jungle.'), nl.
describe(jungle2) :- write('You are in the jungle.'), nl.
describe(dark_jungle1) :- write('You entered the very dense jungle.'), nl.
describe(dark_jungle2) :- write('You entered the very dense jungle.'), nl.
describe(beach1) :- write('You are on the beach.'), nl.
describe(beach2) :- write('You are on the beach.'), nl.
describe(beach3) :- write('You are on the beach.'), nl.
describe(meadow1) :- write('You are in a meadow with a huge wooden totem in the middle.'), nl.
describe(meadow2) :- write('You see a pond in the middle of a flower meadow.'), nl.
describe(forest1) :- write('You are in the forest.'), nl.
describe(forest2) :- write('You are in the forest.'), nl.
describe(fields2) :- write('You are in the fields. You see fallen trees, scorched earth, and a plane wreck to the north.'), nl.
describe(fields1) :- write('You are in the fields.'), nl.
describe(plane_wreck) :- write('You entered the plane wreck. Certainly, more than one person flew on it, unfortunately you do not see anyone.'), nl.
describe(tunnel_entrance) :- write('You entered a dark tunnel.'), nl.
describe(tunnel1) :- write('You are in a long rock corridor. You don''t see much.'), nl.
describe(tunnel2) :- write('You reached a fork. The walls of the tunnel have a strange color...'), nl.
describe(ancient_ruins) :- write('You are in a stone crypt. You can see strange signs on the walls, and in the middle - a rock statue of a knight.'), nl.
describe(volcano_peak) :- write('You reached the top. But it is not the top of the mountain. This is the top of the volcano!'), nl.
describe(mountain_path1) :- write('You are getting closer to the top of the mountain. It''s getting hot...'), nl.
describe(mountain_path2) :- write('You start climbing the mountain path.'), nl.
describe(cave) :- write('You have entered a dark cave. You are clearly not alone here ...'), nl.
describe(ocean1) :- write('You are at the ocean. You see another island in the distance ...'), nl.
describe(ocean2) :- write('You are at the ocean. You see another island in the distance ...'), nl.
describe(ocean3) :- write('You are at the ocean. You see another island in the distance ...'), nl.
describe(beach1) :- write('You are on a sandy beach.'), nl.
describe(beach2) :- write('You are on a sandy beach.'), nl.
describe(beach3) :- write('You are on a sandy beach.'), nl.
describe(house1) :- write('You entered an inhabited house.'), nl.
describe(house2) :- write('You entered a wooden hut. Probably a weaver house.'), nl.
describe(path1) :- write('You are on the path. You don''t know where it leads yet.'), nl.
describe(path2) :- write('You are on the path. It starts to turn north and seems to lead to the top of the mountain.'), nl.
describe(path3) :- write('You are on the path. You don''t know where it leads yet.'), nl.
describe(village1) :- write('You are in the village.'), nl.
describe(village2) :- write('You are in the village.'), nl.
describe(old_house) :- write('You entered the old house. It is probably a forge.'), nl.
describe(well_square) :- write('You are in the main part of the village.'), nl.
describe(crossroads) :- write('You are at a crossroads. Fortunately, someone put up a signpost here.'), nl.
describe(pantry) :- write('You entered the pantry. Many shelves are empty.'), nl.
describe(basement) :- write('You entered the dark basement.'), nl.

/* These rules describe the various objects */

describe(phone) :- write('Works, but no signal.'), nl.
describe(ax) :- write('Old, but perfect for cutting trees.'), nl.
describe(pickax) :- write('Slightly rusty, but still fit for work.'), nl.
describe(map) :- write('Thanks to the map, you can discover and remember new areas on the island.'), nl.
describe(flaming_torch) :- write('The light of this torch can light up the darkness.'), nl.
describe(signpost) :- write('North - trees, East - hot mountaint, South - people, West - a lot of water'), nl.
describe(rose) :- write('It''s beautiful flower.'), nl.
describe(dandelion) :- write('Common flower.'), nl.
describe(well) :- write('The winch works, but the bucket is missing.'), nl.

/* These rules describe the various NPCs */

describe(ancient_guard) :- write('The statue shows a knight in full armor. However, his hands are empty...'), nl.
describe(monkey) :- write('It''s definitely a bad monkey. Why?'), nl. 

/* These rules describe the dialogs with NPCs */

speak(native) :- write('Hello Stranger. You seem like a good man. I have a request for you. My father lives in a cave in the north of the island.'),
                write('I''d like to take him some meat, but I haven''t had time for that lately. Could you do it for me? My father will be grateful.'),
                write('You can get the meat from the cellar. By the way ... I would like to give my chosen one a little thing, but I have no idea.'),
                write('Could you please find something for me? I''m afraid to walk in the jungle. If you help me, I''ll give you my old map.'), 
                write('Maybe it will be useful to you. I can also exchange my ax for some interesting item.'), nl.
speak(old_native) :- write('Hello friend. I am m''Ilio.'), 
                write(' I used to be a village chief, but I stepped back into the shadows after losing the battle with the invaders.'),
                write('I would like to leave this cave, but I am afraid of the reaction of the other inhabitants.'),
                write('However, I will be very grateful to you if you bring me something that will make me remember about the outside world even for a short time.'),
                write('I am also very hungry. My son brings me food sometimes, but it''s usually fruit or nuts. I want meat.'),
                write('Bring them to me and I will reward you.'), nl.
speak(monkey) :- write('U-u-aaaa! Buaaaaa!'), nl.
speak(ancient_guard) :- write('Hello traveler. I used to be a ruthless knight but was cursed by the village shaman.'),
                        write('Now I am only a stone statue. The shaman said the curse would be lifted if I became a "real warrior".'),
                        write('I do not know what it means. Help me please and I will reward you generously.'), nl.
speak(blacksmith) :- write('Hello traveler. I am a local blacksmith.'),
                write('I could forge a simple sword for you or let you use my workshop, but unfortunately I don''t have the right resources.'),
                write('I heard that there are still deposits of iron left in the cave in the north of the island.'),
                write('If you give me a fish, I will give you my pickaxe so that you can mine the ore.'), nl.