Object subclass: Game [
    | isGameOver currentLocation playerObjects recipes locations |

    | meadow1 forest1 darkJungle1 tunnelEntrance ancientRuin volcanoPeak |
    | forest2 meadow2 jungle1 tunnel1 tunnel2 mountainPath1 |
    | fields1 planeWreck jungle2 darkJungle2 cave mountainPath2 |
    | ocean1 beach1 fields2 crossroads path1 path3 path2 mountainPath2 |
    | ocean2 beach2 house1 village1 wellSquare village2 |
    | ocean3 beach3 pantry house2 oldHouse basement |

    | totem tree stick bananaTree lavaSource rose dandelion bush note1 note2 note3 note4 note5 ironOre notebook |
    | palm signpost stone well string wreck meat cloth sheets blastFurnace anvil banana wood mast brushwood torch |
    | flamingTorch rawIron fishingRod liquidIron swordForm fish waterBucket hardwood deck sail raft people sword hotSword |
    | rope ax map mysteriousStone bucket pickax phone pond |
    
    | ancientGuard monkey oldNative native blacksmith |

    | recipeBanana recipeWood recipeMast recipeBrushwood1 recipeBrushwood2 recipeTorch1 recipeTorch2 recipeFlamingTorch1 |
    | recipeRawIron recipeFishingRod recipeLiquidIron recipeSwordForm recipeFish recipeWaterBucket1 recipeWaterBucket2 |
    | recipeHardwood recipeDeck recipeSail recipeRaft recipeFlamingTorch2 recipePeople recipeSword1 recipeSword2 recipeHotSword |
    
    | exchangeRope exchangeAx exchangeMap exchangeMysteriousStone exchangeBanana exchangeBucket exchangePickax exchangeSword |

    "texts"
    introductionText := #(
    ).

    instructionsText := #(
        'Available commands are:'
        'start                  -- to start the game.'
        'n  s  e  w             -- to go in that direction.'
        'take Object            -- to pick up an object.'
        'drop Object            -- to put down an object.'
        'examine Object/NPC     -- to examine an object or NPC.'
        'use Object Object      -- to use the objects together.'
        'inventory              -- to see the objects you are holding.'
        'look                   -- to inspect current location.'
        'look_around            -- to see where you can go.'
        'talk NPC               -- to talk with NPC.'
        'give Object NPC        -- to give an object to NPC.'
        'instructions           -- to see these instructions.'
        'leave                  -- to leave an island.'
        'halt                   -- to end the game and quit.'
        ''
    ).
 
    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    init [
        "objects"
        totem := GameObject newGameObject: 'totem' desc: '' pick: false use: true. 
        tree := GameObject newGameObject: 'tree' desc: '' pick: false use: false.
        stick := GameObject newGameObject: 'stick' desc: '' pick: true use: false.
        bananaTree := GameObject newGameObject: 'banana_tree' desc: '' pick: false use: false.
        lavaSource := GameObject newGameObject: 'lava_source' desc: '' pick: false use: true.
        rose := GameObject newGameObject: 'rose' desc: 'It''s beautiful flower.' pick: true use: false.
        dandelion := GameObject newGameObject: 'dandelion' desc: 'Common flower.' pick: true use: false.
        bush := GameObject newGameObject: 'bush' desc: '' pick: true use: false.
        note1 := GameObject newGameObject: 'note1' 
            desc: '"We got to the village. The people are primitive, but they understand our language very well. He told us to go to the "hot mountain". Perhaps we will be able to call for help from on high."' pick: true use: false.
        note2 := GameObject newGameObject: 'note2' 
            desc: '"We reached the top of the "hot mountain", which turned out to be an active volcano. We tried sending smoke signs but unfortunately it didn''t help. We are now going to the cave at the foot of the volcano. Apparently a wise man lives there, and besides (unreadable)"' pick: true use: false.
        note3 := GameObject newGameObject: 'note3' 
            desc: '"We talked to the old native in the cave. He instructed us to go to a nearby island. He is said to be more civilized than this one. We saw her from the top of the volcano. We just need to build a ship. We go west, where there is the best access to the ocean."' pick: true use: false.
        note4 := GameObject newGameObject: 'note4' 
            desc: '"We cut down some trees. One of them was really big. It grew next to a primitive totem in the north of the island. The natives mentioned something as a special tree, but we didn''t care. We are going to the ocean now to sail to another island."' pick: true use: false.
        note5 := GameObject newGameObject: 'note5' 
            desc: '"We built a boat, we''re le" The note ends in half a sentence.' pick: true use: false.
        ironOre := GameObject newGameObject: 'iron_ore' desc: '' pick: false use: false.
        notebook := GameObject newGameObject: 'notebook' 
            desc: 'This looks like the diary of one of the passengers. The handwriting is very blurry. "We have a damaged engine, the pilot says we have to make an emergency landing on (unreadable)." "We crashed, only three of us survived. Our little group of survivors will try to find rescue. We tried to find all (unreadable), but we may have missed someone. So I leave this (unreadable). If you read this - find us. We''re going (unreadable)." The next pages are torn out.' pick: true use: false.
        palm := GameObject newGameObject: 'palm' desc: '' pick: false use: false.
        signpost := GameObject newGameObject: 'signpost' desc: 'North - trees, East - hot mountaint, South - people, West - a lot of water' pick: false use: false.
        stone := GameObject newGameObject: 'stone' desc: '' pick: true use: false.
        well := GameObject newGameObject: 'well' desc: '' pick: false use: true.
        string := GameObject newGameObject: 'string' desc: '' pick: true use: false.
        wreck := GameObject newGameObject: 'wreck' desc: 'A very primitive boat made up of several logs and lianas. But it looks like it was burnt?' pick: false use: false.
        meat := GameObject newGameObject: 'meat' desc: '' pick: true use: false.
        cloth := GameObject newGameObject: 'cloth' desc: 'It seems to be flammable.' pick: true use: false.
        sheets := GameObject newGameObject: 'sheets' desc: '' pick: true use: false.
        blastFurnace := GameObject newGameObject: 'blast_furnace' desc: '' pick: false use: true.
        anvil := GameObject newGameObject: 'anvil' desc: '' pick: false use: true.
        banana := GameObject newGameObject: 'banana' desc: '' pick: true use: false.
        wood := GameObject newGameObject: 'wood' desc: '' pick: true use: false.
        mast := GameObject newGameObject: 'mast' desc: '' pick: true use: false.
        brushwood := GameObject newGameObject: 'brushwood' desc: 'A bunch of dry branches.' pick: true use: false.
        torch := GameObject newGameObject: 'torch' desc: '' pick: true use: false.
        flamingTorch := GameObject newGameObject: 'flaming_torch' desc: 'The light of this torch can light up the darkness.' pick: true use: false.
        rawIron := GameObject newGameObject: 'raw_iron' desc: '' pick: true use: false.
        fishingRod := GameObject newGameObject: 'fishing_rod' desc: '' pick: true use: false.
        liquidIron := GameObject newGameObject: 'liquid_iron' desc: '' pick: true use: false.
        swordForm := GameObject newGameObject: 'sword_form' desc: '' pick: true use: false.
        fish := GameObject newGameObject: 'fish' desc: '' pick: true use: false.
        waterBucket := GameObject newGameObject: 'water_bucket' desc: '' pick: true use: false.
        hardwood := GameObject newGameObject: 'hardwood' desc: '' pick: true use: false.
        deck := GameObject newGameObject: 'deck' desc: '' pick: true use: false.
        sail := GameObject newGameObject: 'sail' desc: '' pick: true use: false.
        raft := GameObject newGameObject: 'raft' desc: '' pick: true use: false.
        people := GameObject newGameObject: 'people' 
            desc: 'Thank God you''re here! This tree that we cut down was really magical! When we tried to launch our boat, a big storm broke out. Lightning struck all around us, and finally one hit our boat and sent us to nothingness. You save our lives.' pick: true use: false.
        sword := GameObject newGameObject: 'sword' desc: '' pick: true use: true.
        hotSword := GameObject newGameObject: 'hot_sword' desc: '' pick: true use: false.
        rope := GameObject newGameObject: 'rope' desc: '' pick: true use: false.
        ax := GameObject newGameObject: 'ax' desc: 'Old, but perfect for cutting trees.' pick: true use: true.
        map := GameObject newGameObject: 'map' desc: 'Thanks to the map, you can discover and remember new areas on the island.' pick: true use: false.
        mysteriousStone := GameObject newGameObject: 'mysterious_stone' desc: '' pick: true use: false.
        bucket := GameObject newGameObject: 'bucket' desc: '' pick: true use: false.
        pickax := GameObject newGameObject: 'pickax' desc: 'Slightly rusty, but still fit for work.' pick: true use: true.
        phone := GameObject newGameObject: 'phone' desc: 'Works, but no signal.' pick: true use: false.
        pond := GameObject newGameObject: 'pond' desc: '' pick: false use: true.

        "exchanges"
        exchangeRope := Exchange newExchange: banana offerObject: rope 
            desc: 'This monkey was obviously just hungry. Monkey took the fruit and walked away. You can safely take the rope it was hanging on.'.
        exchangeAx := Exchange newExchange: phone offerObject: ax 
            desc: 'Whoa! Shiny! Interesting. Take my ax.'.
        exchangeMap := Exchange newExchange: rose offerObject: ax 
            desc: 'Beautiful! Great idea! Thank you. Take this map. Thanks to it you will not get lost.'.
        exchangeMysteriousStone := Exchange newExchange: sword offerObject: mysteriousStone 
            desc: 'Yes! Now I feel like I can go in peace! So that''s what I missed! Thank you! Please take this stone. The ghosts told me to keep an eye on him. Goodbye...'.
        exchangeBanana := Exchange newExchange: meat offerObject: banana 
            desc: 'Delicious! Thank you! In return, I can give you this fruit that they keep feeding me.'.
        exchangeBucket := Exchange newExchange: dandelion offerObject: bucket 
            desc: 'Thank you friend. An ordinary flower, and so pleasing to the eye. It''s not much, but that''s all I can give you.'.
        exchangePickax := Exchange newExchange: fish offerObject: pickax 
            desc: 'It looks delicious! Thank you! Take my pickaxe and mine some ore.'.
        exchangeSword := Exchange newExchange: rawIron offerObject: sword 
            desc: 'Looks like high quality ore. I''m about to forge a decent sword out of it.'.

        "NPCs"
        ancientGuard := NPC newNPC: 'ancient_guard'
            desc: 'The statue shows a knight in full armor. However, his hands are empty...' 
            speech: 'Hello traveler. I used to be a ruthless knight but was cursed by the village shaman. Now I am only a stone statue. The shaman said the curse would be lifted if I became a "real warrior". I do not know what it means. Help me please and I will reward you generously.' 
            exchange: {exchangeMysteriousStone.}.
        monkey := NPC newNPC: 'monkey' 
            desc: 'It''s definitely a bad monkey. Why? Interestingly, instead of on the liana, the monkey hangs on a fairly solid rope.' 
            speech: 'U-u-aaaa! Buaaaaa!' exchange: {exchangeRope}.
        oldNative := NPC newNPC: 'old_native' desc: '' 
            speech: 'Hello friend. I am m''Ilio. I used to be a village chief, but I stepped back into the shadows after losing the battle with the invaders. I would like to leave this cave, but I am afraid of the reaction of the other inhabitants. However, I will be very grateful to you if you bring me something that will make me remember about the outside world even for a short time. I am also very hungry. My son brings me food sometimes, but it''s usually fruit or nuts. I want meat. Bring them to me and I will reward you.' 
            exchange: {exchangeBanana. exchangeBucket}.
        native := NPC newNPC: 'native' desc: '' 
            speech: 'Hello Stranger. You seem like a good man. I have a request for you. My father lives in a cave in the north of the island. I''d like to take him some meat, but I haven''t had time for that lately. Could you do it for me? My father will be grateful. You can get the meat from the cellar. By the way... I would like to give my chosen one a little thing, but I have no idea. Could you please find something for me? I''m afraid to walk in the jungle. If you help me, I''ll give you my old map. Maybe it will be useful to you. I can also exchange my ax for some interesting item.' 
            exchange: {exchangeAx. exchangeMap}.
        blacksmith := NPC newNPC: 'blacksmith' desc: '' 
            speech: 'Hello traveler. I am a local blacksmith. I could forge a simple sword for you or let you use my workshop, but unfortunately I don''t have the right resources. I heard that there are still deposits of iron left in the cave in the north of the island. If you give me a fish, I will give you my pickaxe so that you can mine the ore.' 
            exchange: {exchangePickax. exchangeSword}.

        "recipes"
        recipeBanana := Recipe newRecipe: stone gameObject2: bananaTree res: banana.
        recipeWood := Recipe newRecipe: ax gameObject2: tree res: wood.
        recipeMast := Recipe newRecipe: ax gameObject2: palm res: mast.
        recipeBrushwood1 := Recipe newRecipe: ax gameObject2: bush res: brushwood.
        recipeBrushwood2 := Recipe newRecipe: sword gameObject2: bush res: brushwood.
        recipeTorch1 := Recipe newRecipe: stick gameObject2: cloth res: torch.
        recipeTorch2 := Recipe newRecipe: brushwood gameObject2: cloth res: torch.
        recipeFlamingTorch1 := Recipe newRecipe: torch gameObject2: lavaSource res: flamingTorch.
        recipeRawIron := Recipe newRecipe: ironOre gameObject2: pickax res: rawIron.
        recipeFishingRod := Recipe newRecipe: string gameObject2: stick res: fishingRod.
        recipeLiquidIron := Recipe newRecipe: rawIron gameObject2: blastFurnace res: liquidIron.
        recipeSwordForm := Recipe newRecipe: stick gameObject2: liquidIron res: swordForm.
        recipeFish := Recipe newRecipe: fishingRod gameObject2: pond res: fish.
        recipeWaterBucket1 := Recipe newRecipe: bucket gameObject2: well res: waterBucket.
        recipeWaterBucket2 := Recipe newRecipe: bucket gameObject2: pond res: waterBucket.
        recipeHardwood := Recipe newRecipe: wood gameObject2: wood res: hardwood.
        recipeDeck := Recipe newRecipe: hardwood gameObject2: rope res: deck.
        recipeSail := Recipe newRecipe: mast gameObject2: sheets res: sail.
        recipeRaft := Recipe newRecipe: sail gameObject2: deck res: raft.
        recipeFlamingTorch2 := Recipe newRecipe: torch gameObject2: blastFurnace res: flamingTorch.
        recipePeople := Recipe newRecipe: mysteriousStone gameObject2: totem res: people.
        recipeSword1 := Recipe newRecipe: hotSword gameObject2: waterBucket res: sword.
        recipeSword2 := Recipe newRecipe: hotSword gameObject2: pond res: sword.
        recipeHotSword := Recipe newRecipe: swordForm gameObject2: anvil res: hotSword.
        
        "locations"
        meadow1 := Location newLocation: 1 newName: 'meadow1' desc: 'You are in a meadow with a huge wooden totem in the middle.' 
                    northLoc: nil eastLoc: 2 southLoc: 7 westLoc: nil locGameObjects: {totem.} locNPCs: {} isDark: false.
        forest1 := Location newLocation: 2 newName: 'forest' desc: 'You are in the forest.' 
                    northLoc: nil eastLoc: 3 southLoc: 8 westLoc: 1 locGameObjects: {tree. tree. stick.} locNPCs: {} isDark: false.
        darkJungle1 := Location newLocation: 3 newName: 'dark_jungle1' desc: 'You entered the very dense jungle.' 
                    northLoc: nil eastLoc: 4 southLoc: 9 westLoc: 2 locGameObjects: {bananaTree} locNPCs: {} isDark: true.
        tunnelEntrance := Location newLocation: 4 newName: 'tunnel_entrance' desc: 'You entered a dark tunnel.' 
                    northLoc: nil eastLoc: nil southLoc: 10 westLoc: 3 locGameObjects: {} locNPCs: {} isDark: true.
        ancientRuin := Location newLocation: 5 newName: 'ancient_ruin' desc: 'You are in a stone crypt. You can see strange signs on the walls, and in the middle - a rock statue of a knight.' 
                    northLoc: nil eastLoc: nil southLoc: 11 westLoc: nil locGameObjects: {} locNPCs: {ancientGuard.} isDark: true.
        volcanoPeak := Location newLocation: 6 newName: 'volcano_peak' desc: 'ou reached the top. But it is not the top of the mountain. This is the top of the volcano!' 
                    northLoc: nil eastLoc: nil southLoc: 12 westLoc: nil locGameObjects: {lavaSource.} locNPCs: {} isDark: false.
        forest2 := Location newLocation: 7 newName: 'forest2' desc: 'You are in the forest.' 
                    northLoc: 1 eastLoc: 8 southLoc: 13 westLoc: nil locGameObjects: {tree. tree.} locNPCs: {} isDark: false.
        meadow2 := Location newLocation: 8 newName: 'meadow2' desc: 'You see a pond in the middle of a flower meadow.' 
                    northLoc: 2 eastLoc: 9 southLoc: nil westLoc: 7 locGameObjects: {pond. rose. dandelion.} locNPCs: {} isDark: false.
        jungle1 := Location newLocation: 9 newName: 'jungle1' desc: 'You are in the jungle.' 
                    northLoc: 3 eastLoc: nil southLoc: 15 westLoc: 8 locGameObjects: {bush. tree.} locNPCs: {monkey.} isDark: false.
        tunnel1 := Location newLocation: 10 newName: 'tunnel1' desc: 'You are in a long rock corridor. You don''t see much.' 
                    northLoc: 4 eastLoc: 11 southLoc: nil westLoc: nil locGameObjects: {note3.} locNPCs: {} isDark: true.
        tunnel2 := Location newLocation: 11 newName: 'tunnel2' desc: 'You reached a fork. The walls of the tunnel have a strange color...' 
                    northLoc: 5 eastLoc: nil southLoc: 17 westLoc: 10 locGameObjects: {ironOre.} locNPCs: {} isDark: true.
        mountainPath1 := Location newLocation: 12 newName: 'mountain_path1' desc: 'You are getting closer to the top of the mountain. It''s getting hot...' 
                    northLoc: 6 eastLoc: nil southLoc: 18 westLoc: nil locGameObjects: {note2.} locNPCs: {} isDark: false.
        fields1 := Location newLocation: 13 newName: 'fields1' desc: 'You are in the fields.' 
                    northLoc: 7 eastLoc: nil southLoc: 20 westLoc: nil locGameObjects: {note4.} locNPCs: {} isDark: false.
        planeWreck := Location newLocation: 14 newName: 'plane_wreck' desc: 'You entered the plane wreck. Certainly, more than one person flew on it, unfortunately you do not see anyone.' 
                    northLoc: nil eastLoc: nil southLoc: 21 westLoc: nil locGameObjects: {notebook.} locNPCs: {} isDark: false.
        jungle2 := Location newLocation: 15 newName: 'jungle2' desc: 'You are in the jungle.' 
                    northLoc: 9 eastLoc: 16 southLoc: 22 westLoc: nil locGameObjects: {bush.} locNPCs: {} isDark: false.
        darkJungle2 := Location newLocation: 16 newName: 'dark_jungle2' desc: 'You entered the very dense jungle.' 
                    northLoc: nil eastLoc: nil southLoc: 23 westLoc: 15 locGameObjects: {bush. stick.} locNPCs: {} isDark: true.
        cave := Location newLocation: 17 newName: 'cave' desc: 'You have entered a dark cave. You are clearly not alone here...' 
                    northLoc: 11 eastLoc: nil southLoc: nil westLoc: nil locGameObjects: {} locNPCs: {oldNative.} isDark: true.
        mountainPath2 := Location newLocation: 18 newName: 'mountain_path2' desc: 'You start climbing the mountain path.' 
                    northLoc: 12 eastLoc: nil southLoc: 25 westLoc: nil locGameObjects: {} locNPCs: {} isDark: false.
        ocean1 := Location newLocation: 19 newName: 'ocean1' desc: 'You are at the ocean. You see another island in the distance...' 
                    northLoc: nil eastLoc: 20 southLoc: nil westLoc: nil locGameObjects: {} locNPCs: {} isDark: false.
        beach1 := Location newLocation: 20 newName: 'beach1' desc: 'You are on a sandy beach.' 
                    northLoc: 13 eastLoc: 21 southLoc: 27 westLoc: 19 locGameObjects: {palm.} locNPCs: {} isDark: false.
        fields2 := Location newLocation: 21 newName: 'fields2' desc: 'You are in the fields. You see fallen trees, scorched earth, and a plane wreck to the north.' 
                    northLoc: 14 eastLoc: 22 southLoc: nil westLoc: 20 locGameObjects: {} locNPCs: {} isDark: false.
        crossroads := Location newLocation: 22 newName: 'crossroads' desc: 'You are at a crossroads. Fortunately, someone put up a signpost here.' 
                    northLoc: 15 eastLoc: 23 southLoc: 29 westLoc: 21 locGameObjects: {signpost.} locNPCs: {} isDark: false.
        path1 := Location newLocation: 23 newName: 'path1' desc: 'You are on the path. You don''t know where it leads yet.' 
                    northLoc: 16 eastLoc: 24 southLoc: 30 westLoc: 22 locGameObjects: {} locNPCs: {} isDark: false.
        path3 := Location newLocation: 24 newName: 'path3' desc: 'You are on the path. You don''t know where it leads yet.' 
                    northLoc: nil eastLoc: 25 southLoc: 31 westLoc: 23 locGameObjects: {} locNPCs: {} isDark: false.
        path2 := Location newLocation: 25 newName: 'path2' desc: 'You are on the path. It starts to turn north and seems to lead to the top of the mountain.' 
                    northLoc: 18 eastLoc: nil southLoc: nil westLoc: 24 locGameObjects: {stone.} locNPCs: {} isDark: false.
        ocean2 := Location newLocation: 26 newName: 'ocean2' desc: 'You are at the ocean. You see another island in the distance...' 
                    northLoc: nil eastLoc: 27 southLoc: nil westLoc: nil locGameObjects: {} locNPCs: {} isDark: false.
        beach2 := Location newLocation: 27 newName: 'beach2' desc: 'You are on a sandy beach.' 
                    northLoc: 20 eastLoc: nil southLoc: 33 westLoc: 26 locGameObjects: {palm. palm. stick.} locNPCs: {} isDark: false.
        house1 := Location newLocation: 28 newName: 'house1' desc: 'You entered an inhabited house.' 
                    northLoc: nil eastLoc: 29 southLoc: 34 westLoc: nil locGameObjects: {} locNPCs: {native.} isDark: false.
        village1 := Location newLocation: 29 newName: 'village1' desc: 'You are in the village.' 
                    northLoc: 22 eastLoc: 30 southLoc: 35 westLoc: 28 locGameObjects: {note1.} locNPCs: {} isDark: false.
        wellSquare := Location newLocation: 30 newName: 'well_square' desc: 'You are in the main part of the village.' 
                    northLoc: 23 eastLoc: 31 southLoc: 36 westLoc: 29 locGameObjects: {well.} locNPCs: {} isDark: false.
        village2 := Location newLocation: 31 newName: 'village2' desc: 'You are in the village.' 
                    northLoc: 24 eastLoc: nil southLoc: nil westLoc: 30 locGameObjects: {string.} locNPCs: {} isDark: false.
        ocean3 := Location newLocation: 32 newName: 'ocean3' desc: 'You are at the ocean. You see another island in the distance...' 
                    northLoc: nil eastLoc: 33 southLoc: nil westLoc: nil locGameObjects: {note5. wreck.} locNPCs: {} isDark: false.
        beach3 := Location newLocation: 33 newName: 'beach3' desc: 'You are on a sandy beach.' 
                    northLoc: 27 eastLoc: nil southLoc: nil westLoc: 32 locGameObjects: {palm.} locNPCs: {} isDark: false.
        pantry := Location newLocation: 34 newName: 'pantry' desc: 'You entered the pantry. Many shelves are empty.' 
                    northLoc: 28 eastLoc: nil southLoc: nil westLoc: nil locGameObjects: {meat.} locNPCs: {} isDark: true.
        house2 := Location newLocation: 35 newName: 'house2' desc: 'You entered a wooden hut. Probably a weaver house.' 
                    northLoc: 29 eastLoc: nil southLoc: nil westLoc: nil locGameObjects: {cloth. sheets.} locNPCs: {} isDark: false.
        oldHouse := Location newLocation: 36 newName: 'old_house' desc: 'You entered the old house. It is probably a forge.' 
                    northLoc: 30 eastLoc: 37 southLoc: nil westLoc: nil locGameObjects: {blastFurnace.} locNPCs: {blacksmith.} isDark: false.
        basement := Location newLocation: 37 newName: 'basement' desc: 'You entered the dark basement.' 
                    northLoc: nil eastLoc: nil southLoc: nil westLoc: 36 locGameObjects: {anvil.} locNPCs: {} isDark: true.

        currentLocation := fields2.

        isGameOver := false.
        playerObjects := OrderedCollection new.
        playerObjects add: phone.

        recipes := {recipeBanana. recipeWood. recipeMast. recipeBrushwood1. recipeBrushwood2. recipeTorch1. recipeTorch2. recipeFlamingTorch1.
                    recipeRawIron. recipeFishingRod. recipeLiquidIron. recipeSwordForm. recipeFish. recipeWaterBucket1. recipeWaterBucket2.
                    recipeHardwood. recipeDeck. recipeSail. recipeRaft. recipeFlamingTorch2. recipePeople. recipeSword1. recipeSword2. recipeHotSword}.

        locations := {meadow1. forest1. darkJungle1. tunnelEntrance. ancientRuin. volcanoPeak.
                    forest2. meadow2. jungle1. tunnel1. tunnel2. mountainPath1. 
                    fields1. planeWreck. jungle2. darkJungle2. cave. mountainPath2. 
                    ocean1. beach1. fields2. crossroads. path1. path3. path2. mountainPath2. 
                    ocean2. beach2. house1. village1. wellSquare. village2. 
                    ocean3. beach3. pantry. house2. oldHouse. basement}.
    ]

    "describing"
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printNames: objects [
        objects do: [ :object | Transcript show: object getName.
                                Transcript show: ', '. ].
        Transcript cr.
    ]

    look: location [
        location isDark & ((self findByName: 'flaming_torch' and: playerObjects) = nil) ifTrue: [
            Transcript show: 'You see darkness only'; cr.
            ^false.
        ].
        Transcript show: location getDescription; cr.
        (location getObjects, location getNPCs) size > 0
        ifTrue: [
            Transcript show: 'There are: '.
            self printNames: location getObjects, location getNPCs.
            ^true.
        ].
        Transcript show: 'There is nothing.'.
        ^true.
    ]

    printObjectDescription: object [
        object getDescription = ''
        ifTrue: [
            Transcript show: 'You don''t know anything about ', object getName; cr.
            ^false.
        ].
        Transcript show: object getDescription; cr.
        ^true.
    ]

    printDirection: direction and: char [
        direction = nil
        ifTrue: [
            ^true.
        ].
        Transcript show: char, ' -> '.
        direction isDiscovered
        ifTrue: [
            Transcript show: direction getName; cr.
        ]
        ifFalse: [
            Transcript show: 'undiscovered'; cr.
        ].
        ^true.
    ]

    lookAround: location[
        | north south west east |
        north := self findLocationByID: location getN.
        south := self findLocationByID: location getS.
        east := self findLocationByID: location getE.
        west := self findLocationByID: location getW.

        self printDirection: north and: 'n'.
        self printDirection: east and: 'e'.
        self printDirection: west and: 'w'.
        self printDirection: south and: 's'.
    ]

    "interacting with NPCs"
    talkNPC: npcName [
        | npc |
        npc := self findByName: npcName and: currentLocation getNPCs.

        npc = nil
        ifTrue: [
            Transcript show: 'There is no ', npcName; cr.
            ^false.
        ].

        npc talk.
        ^true.
    ]

    findExchangeByObject: needObject and: exchanges [
        exchanges do: [ :exchange | exchange getNeededObject = needObject ifTrue:[
            ^exchange.
        ]].
        ^nil.
    ]

    giveNPC: objectName and: npcName [
        | object npc newObject exchange |
        npc := self findByName: npcName and: currentLocation getNPCs.

        npc = nil
        ifTrue: [
            Transcript show: 'There is no ', npcName; cr.
            ^false.
        ].

        object := self findByName: objectName and: playerObjects.
        object = nil
        ifTrue: [
            Transcript show: 'You don''t have ', objectName; cr.
            ^false.
        ].

        exchange := self findExchangeByObject: object and: npc getExchanges.

        exchange = nil
        ifTrue: [
            Transcript show: 'He doesn''t want this'; cr.
            ^false.
        ].

        newObject := exchange getOfferedObject.
        playerObjects add: newObject.
        Transcript show: exchange getDescription; cr.
        Transcript show: 'You got ', newObject getName; cr.
        playerObjects remove: object.
        ^true.
    ]

    "interacting with items"
    printInventory[
        self printNames: playerObjects.
    ]

    examine: objectName [
        | object |
        object := self findByName: objectName and: playerObjects, currentLocation getObjects, currentLocation getNPCs.
        object = nil
        ifTrue: [
            Transcript show: 'There is no ', objectName; cr.
            ^false.
        ].
        self printObjectDescription: object. 
        ^true
    ]

    findByName: name and: anArray [
        anArray do: [ :object | object getName = name ifTrue: [ ^object ]].
        ^nil.
    ]

    take: objectName [
        | object |
        object := self findByName: objectName and: currentLocation getObjects.
        object = nil
        ifTrue: [
            Transcript show: 'There is no ', objectName; cr.
            ^false.
        ].

        object isPickable = true 
        ifFalse: [
            Transcript show: 'You can''t do this.'; cr.
            ^false.
        ].
        playerObjects add: object.
        currentLocation removeObject: object.
        Transcript show: 'You picked up ', objectName; cr.
        ^true.
        
    ]

    drop: objectName [
        | object |
        object := self findByName: objectName and: playerObjects.
        object = nil
        ifTrue: [
            Transcript show: 'You don''t have ', objectName; cr.
            ^false.
        ].

        playerObjects remove: object.
        currentLocation addObject: object.
        Transcript show: 'You dropped ', objectName; cr.
        ^true.  
    ]

    findRecipe: object1 and: object2 [
        recipes do: [ :recipe | 
        ((recipe getObject1 = object1 & recipe getObject2 = object2) |
        (recipe getObject1 = object2 & recipe getObject2 = object1)) ifTrue: [
            ^recipe.
        ]].
        ^nil.
    ]

    isAvailable: name [
        | object |
        object := self findByName: name and: playerObjects, currentLocation getObjects.
        ^object.
    ]

    useUp: object [
        | objectInArray |
        object isReusable
        ifTrue: [
            ^true.
        ].

        objectInArray := self findByName: object getName and: playerObjects.
        objectInArray = nil
        ifTrue: [
            objectInArray := self findByName: object getName and: currentLocation getObjects.
            objectInArray = nil
            ifFalse: [
                currentLocation removeObject: object.
                ^true.
            ].
            ^false.
        ]
        ifFalse: [
            playerObjects remove: object.
            ^true.
        ].
    ]

    use: objectName1 and: objectName2 [
        | object1 object2 recipe result |

        object1 := self isAvailable: objectName1.
        object1 = nil
        ifTrue: [
            Transcript show: 'You don''t have ', objectName1; cr.
            ^false.
        ].

        object2 := self isAvailable: objectName2.
        object2 = nil
        ifTrue: [
            Transcript show: 'You don''t have ', objectName2; cr.
            ^false.
        ].

        recipe := self findRecipe: object1 and: object2.

        recipe = nil 
        ifTrue: [
            Transcript show: 'You can''t use it that way'; cr.
            ^false.
        ].

        self useUp: object1.
        self useUp: object2.

        playerObjects add: recipe getResult.

        Transcript show: 'You got ', recipe getResult getName; cr.
        ^true.
    ]

    "moving"
    findLocationByID: index [
        locations do: [ :location | location getId = index ifTrue: [ ^location ]].
        ^nil.
    ]

    updateLocation: locationIndex [
        | newLocation |
        newLocation := self findLocationByID: locationIndex.
        newLocation = nil
        ifTrue: [
            Transcript show: 'You can''t go that way'; cr.
            ^false.
        ].

        (self findByName: 'map' and: playerObjects) = nil 
        ifFalse: [
            newLocation discover.
        ].
        currentLocation := newLocation.
        self look: currentLocation.
        ^true.
    ]

    "leaving an island"
    hasNotes [
        ^(((self findByName: 'note1' and: playerObjects) ~= nil) &
        ((self findByName: 'note2' and: playerObjects) ~= nil) &
        ((self findByName: 'note3' and: playerObjects) ~= nil) &
        ((self findByName: 'note4' and: playerObjects) ~= nil) &
        ((self findByName: 'note5' and: playerObjects) ~= nil))
    ]

    atOcean [
        ^((currentLocation = ocean1) | (currentLocation = ocean2) | (currentLocation = ocean3))
    ]

    hasRaft [
        ^((self findByName: 'raft' and: playerObjects) ~= nil)
    ]

    hasPeople [
        ^((self findByName: 'people' and: playerObjects) ~= nil)
    ]

    leave [
        (self hasRaft) & (self atOcean)
        ifTrue: [
            (self hasNotes) & (self hasPeople)
            ifTrue: [
                Transcript show: 'You left the island. You rescued rest of the passengers and complete the story. You won!'; cr.
                isGameOver := true.
                ^true.
            ].

            self hasNotes
            ifTrue: [
                Transcript show: 'You left the island. You complete the story, but didn''t find rest of the passengers.'; cr.
                isGameOver := true.
                ^true.
            ].

            self hasPeople
            ifTrue: [
                Transcript show: 'You left the island. You found the rest of the passengers, but didn''t know their full story.'; cr.
                isGameOver := true.
                ^true.
            ].

            Transcript show: 'You left the island, but dont''t know anything.'; cr.
            isGameOver := true.
            ^true.
        ].

        self atOcean
        ifFalse: [
            Transcript show: 'You aren''t at the ocean.'; cr.
            ^false.
        ].

        self hasRaft
        ifFalse: [
            Transcript show: 'You can''t swim across the ocean. You need boat.'; cr.
            ^false.
        ].
    ]
    
    "commands"
    checkNumOfArgs: subs and: num [
        subs size = num
        ifTrue: [
            ^true.
        ]
        ifFalse: [
            Transcript show: 'Wrong number of arguments. Required: ', num printString; cr.
            ^false.
        ].
    ]

    printIntroduction [
       self printLines: introductionText.
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        Transcript cr.
        Transcript show: '> '.
        ^FileStream stdin nextLine
    ]

    "Run the game."
    run [
        | cmd isUnknown input subs |

        self printIntroduction.
        self printInstructions.
        self look: currentLocation.

        "Game loop."
        [isGameOver] whileFalse: [

            isUnknown := true.
            input := self readCommand.
            subs := input substrings.
            cmd := subs first.

            cmd = 'instructions' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self printInstructions.
                ]
            ].

            cmd = 'n' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self updateLocation: currentLocation getN.
                ]
            ].

            cmd = 'e' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self updateLocation: currentLocation getE.
                ]
            ].

            cmd = 's' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self updateLocation: currentLocation getS.
                ]
            ].

            cmd = 'w' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self updateLocation: currentLocation getW.
                ]
            ].

            cmd = 'inventory' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self printInventory.
                ]
            ].

            cmd = 'look' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self look: currentLocation.
                ]
            ].

            cmd = 'look_around' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self lookAround: currentLocation.
                ]
            ].

            cmd = 'examine' 
            ifTrue: [
                | objectName |
                isUnknown := false.
                (self checkNumOfArgs: subs and: 2)
                ifTrue: [
                    objectName := subs at:2.
                    self examine: objectName.
                ]
            ].

            cmd = 'take' 
            ifTrue: [
                | objectName |
                isUnknown := false.
                (self checkNumOfArgs: subs and: 2)
                ifTrue: [
                    objectName := subs at:2.
                    self take: objectName.
                ]
            ].

            cmd = 'drop' 
            ifTrue: [
                | objectName |
                isUnknown := false.
                (self checkNumOfArgs: subs and: 2)
                ifTrue: [
                    objectName := subs at:2.
                    self drop: objectName.
                ]
            ].

            cmd = 'use' 
            ifTrue: [
                | objectName1 objectName2 |
                isUnknown := false.
                (self checkNumOfArgs: subs and: 3)
                ifTrue: [
                    objectName1 := subs at: 2.
                    objectName2 := subs at: 3.
                    self use: objectName1 and: objectName2.
                ]
            ].

            cmd = 'talk' 
            ifTrue: [
                | npcName |
                isUnknown := false.
                (self checkNumOfArgs: subs and: 2)
                ifTrue: [
                    npcName := subs at:2.
                    self talkNPC: npcName.
                ]
            ].

            cmd = 'give' 
            ifTrue: [
                | objectName npcName |
                isUnknown := false.
                (self checkNumOfArgs: subs and: 3)
                ifTrue: [
                    objectName := subs at:2.
                    npcName := subs at:3.
                    self giveNPC: objectName and: npcName.
                ]
            ].

            cmd = 'leave' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self leave.
                ]
            ].

            cmd = 'halt' 
            ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    isGameOver := true.
                ]
            ].

            isUnknown 
            ifTrue: [
                Transcript show: 'Unrecognized command.'; cr.
            ]
        ]
    ]
]

FileStream fileIn: 'object.st'.
FileStream fileIn: 'location.st'.
FileStream fileIn: 'NPC.st'.
FileStream fileIn: 'recipe.st'.
FileStream fileIn: 'exchange.st'.

Game new run.
