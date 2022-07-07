Object subclass: Game [
     | isGameOver |
     | currentLocation |
     | playerObjects |
     | recipes |
     | locations |


    introductionText := #(
    ).

    instructionsText := #(
        'Available commands are:'
        'start                  -- to start the game.'
        'n  s  e  w             -- to go in that direction.'
        'take Object            -- to pick up an object.'
        'drop Object            -- to put down an object.'
        'examine Object         -- to examine an object.'
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
        |phone pen thing sign ax sword rope someplace jungle1 jungle2 jungle3 jungle4 jungle5 jungle6 jungle7 guard eagle native
         jungle1GameObjects jungle1NPCS recipeThing recipeWood flamingTorch ocean1 raft map exchangeAx |
         
        phone := GameObject newGameObject: 'phone' desc: 'Glowing' pick: true use: false.
        pen := GameObject newGameObject: 'pen' desc: 'Shiny' pick: true use: false.
        thing := GameObject newGameObject: 'thing' desc: 'Strange' pick: true use: false.
        sign := GameObject newGameObject: 'sign' desc: 'W - path to mountain peak, N - jungle, E - village, S - path to cave' pick: false use: false.
        ax := GameObject newGameObject: 'ax' desc: 'A sturdy ax for chopping wood.' pick: true use: true.
        sword := GameObject newGameObject: 'sword' desc: '' pick: true use: false.
        rope := GameObject newGameObject: 'rope' desc: 'It is a strong rope. Can be useful' pick: true use: false.
        flamingTorch := GameObject newGameObject: 'flamingTorch' desc: '' pick: true use: false.
        raft := GameObject newGameObject: 'raft' desc: '' pick: true use: false.
        map := GameObject newGameObject: 'map' desc: '' pick: true use: false.

        recipeThing := Recipe newRecipe: phone gameObject2: pen res: thing.
        recipeWood := Recipe newRecipe: ax gameObject2: sign res: thing.

        exchangeAx := Exchange newExchange: phone offerObject: ax descr: 'Prosz'.

        native := Npc newNpc: 'Native' desc: 'You see a native, whose face is obscured by darkness. Try interacting with him, if you''re brave enough.' dialogue: 'Hey you, are you looking for an ax? I will give it to you for something that will entertain my children.' exchange: {exchangeAx}.
        guard := Npc newNpc: 'Guard' desc: 'It looks like someone is guarding the ruins. You should try talking to him, maybe he''ll let you by easily.' dialogue: 'You really thought I would let you in? You have no chance if you''re unarmed.' exchange: {}.

        ocean1 := Location newLocation: 7 newName: 'Ocean' desc: 'You are at the ocean' northLoc: nil westLoc: nil southLoc: nil eastLoc: nil locGameObjects: {} locNpcs: {} isDark: false.
        jungle2 :=  Location newLocation: 2 newName: 'Jungle2' desc: 'You are in the jungle' northLoc: nil westLoc: nil southLoc: nil eastLoc: nil locGameObjects: {} locNpcs: {} isDark: false.
        jungle3 := Location newLocation: 3 newName: 'Jungle3' desc: 'You are in the jungle' northLoc: nil westLoc: nil southLoc: nil eastLoc: nil locGameObjects: {} locNpcs: {} isDark: false.
        jungle4 := Location newLocation: 4 newName: 'Jungle4' desc: 'You are in the jungle' northLoc: nil westLoc: nil southLoc: nil eastLoc: nil locGameObjects: {} locNpcs: {} isDark: false.
        jungle5 := Location newLocation: 5 newName: 'Jungle5' desc: 'You are in the jungle' northLoc: nil westLoc: nil southLoc: nil eastLoc: nil locGameObjects: {} locNpcs: {} isDark: false.
        jungle6 := Location newLocation: 6 newName: 'Jungle6' desc: 'You are in the jungle' northLoc: nil westLoc: nil southLoc: nil eastLoc: nil locGameObjects: {} locNpcs: {} isDark: false.
        jungle1 := Location newLocation: 1 newName: 'Jungle1' desc: 'You are in the jungle1' northLoc: nil westLoc: jungle2 southLoc: 0 eastLoc: nil locGameObjects: {sign.} locNpcs: {native} isDark: false.
        someplace := Location newLocation: 0 newName: 'Someplace' desc: 'You are in someplace' northLoc: 1 westLoc: 7 southLoc: 6 eastLoc: 4 locGameObjects: {thing. phone. map.} locNpcs: {native} isDark: false.
        
        currentLocation:= someplace.

        isGameOver := false.
        playerObjects := OrderedCollection new.
        playerObjects add: phone.
        playerObjects add: pen.
        playerObjects add: raft.

        recipes := {recipeThing. recipeWood}.

        locations := {ocean1. jungle1. jungle2. jungle3. jungle4. jungle5. jungle6. someplace}.
    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printIntroduction [
       self printLines: introductionText.
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    printNames: objects [
        objects do: [ :object | Transcript show: object getName; cr ]
    ]

    printInventory: anInventory [
        Transcript show: 'Your inventory:'; cr.
        self printNames: anInventory.
    ]

    printLocation: location [
        location isDark & ((self findByName: 'flamingTorch' and: playerObjects) = nil) ifTrue: [
            Transcript show: 'You see darkness only'; cr.
            ^false.
        ].
        Transcript show: location getDescription; cr.
        Transcript show: 'You see:'; cr.
        self printNames: location getGameObjects.
        self printNames: location getNpcs.
        ^true.
    ]

    printGameObjectDescription: object [
        Transcript show: object getDescription; cr.
    ]

    examine: objectName [
        playerObjects do: [:object | object getName = objectName ifTrue: [self printGameObjectDescription: object. ^true]].
        currentLocation getGameObjects do: [:object | object getName = objectName ifTrue: [self printGameObjectDescription: object. ^true]].
    ]

    findByName: name and: anArray [
        anArray do: [:object | object getName = name ifTrue: [ ^object ]].
        ^nil.
    ]

    findLocationByIndex: index [
        locations do: [:location | location getId = index ifTrue: [ ^location ]].
        ^nil.
    ]

    lookAround: location[
        | north south west east |
        north := self findLocationByIndex: location getN.
        south := self findLocationByIndex: location getS.
        east := self findLocationByIndex: location getE.
        west := self findLocationByIndex: location getW.

          north = nil
          ifFalse: [
               Transcript show: 'north -> '.
               north isDiscovered
               ifTrue: [
                    Transcript show: north getName; cr.
               ]
               ifFalse: [
                    Transcript show: 'undiscovered'; cr.
               ]
          ].
          east = nil
          ifFalse: [
               Transcript show: 'east -> '.
               east isDiscovered
               ifTrue: [
                    Transcript show: east getName; cr.
               ]
               ifFalse: [
                    Transcript show: 'undiscovered'; cr.
               ]
          ].
          west = nil
          ifFalse: [
               Transcript show: 'west -> '.
               west isDiscovered
               ifTrue: [
                    Transcript show: west getName; cr.
               ]
               ifFalse: [
                    Transcript show: 'undiscovered'; cr.
               ]
          ].
          south = nil
          ifFalse: [
               Transcript show: 'south -> '.
               south isDiscovered
               ifTrue: [
                    Transcript show: south getName; cr.
               ]
               ifFalse: [
                    Transcript show: 'undiscovered'; cr.
               ]
          ].
    ]
    
    updateLocation: locationIndex [
        | newLocation |
        newLocation := self findLocationByIndex: locationIndex.
        newLocation = nil
        ifTrue: [
            Transcript show: 'You cannot go that way'; cr.
            ^false.
        ].

        (self findByName: 'map' and: playerObjects) = nil 
        ifFalse: [
            newLocation discover.
        ].
        currentLocation := newLocation.
        self printLocation: currentLocation.
        ^true.
    ]

    pick: objectName [
        | its i |
        its := currentLocation getGameObjects.
        i := self findByName: objectName and: its.
        i = nil
        ifTrue: [
            Transcript show: 'There is no  '.
            Transcript show: objectName; cr.
            ^false.
        ].

        i isPickable = true 
        ifFalse: [
            Transcript show: 'You cannot pick '.
            Transcript show: objectName; cr.
            ^false.
        ].
        playerObjects add: i.
        currentLocation removeGameObject: i.
        ^true.
        
    ]

    findRecipe: object1 and: object2 [
        recipes do: [ :recipe | 
        ((recipe getObject1 = object1 & recipe getObject2 = object2) |
        (recipe getObject1 = object2 & recipe getObject1 = object2)) ifTrue: [
            ^recipe.
        ]].
        ^nil.
    ]

    isAvailable: name and: objects [
        objects do: [ :object | object getName = name ifTrue: [
            ^object.
        ]].
        ^nil.
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
            objectInArray := self findByName: object getName and: currentLocation getGameObjects.
            objectInArray = nil
            ifFalse: [
                currentLocation removeGameObject: object.
                ^true.
            ].
            ^false.
        ]
        ifFalse: [
            playerObjects := playerObjects copyWithout: object.
            ^true.
        ].
    ]

    use: objectName1 and: objectName2 [
        |object1 object2 recipe result objectLoc|

        object1 := self isAvailable: objectName1 and: playerObjects, currentLocation getGameObjects.
        object1 = nil
        ifTrue: [
            Transcript show: 'You don''t have '.
            Transcript show: objectName1; cr.
            ^false.
        ].

        object2 := self isAvailable: objectName2 and: playerObjects, currentLocation getGameObjects.
        object2 = nil
        ifTrue: [
            Transcript show: 'You don''t have '.
            Transcript show: objectName2; cr.
            ^false.
        ].

        "objectLoc := false.

        object1 := self findByName: objectName1 and: playerObjects.

        object1 = nil
        ifTrue: [
            Transcript show: 'You do not have '.
            Transcript show: objectName1; cr.
            ^false.
        ].

        object2 := self findByName: objectName2 and: playerObjects.

        object2 = nil
        ifTrue: [
            object2 := self findByName: objectName2 and: currentLocation getGameObjects.

            object2 = nil
            ifTrue: [
                Transcript show: 'You do not have '.
                Transcript show: objectName2; cr.
                ^false.
            ].

            objectLoc := true.
        ]."

        recipe := self findRecipe: object1 and: object2.

        recipe = nil 
        ifTrue: [
            Transcript show: 'You cannot do that'; cr.
            ^false.
        ].

        "object1 isReusable
        ifFalse: [
            playerObjects := playerObjects copyWithout: object1.
        ].
        objectLoc
        ifTrue: [
            object2 isReusable
            ifFalse: [
            	currentLocation removeGameObject: object2.
            ]
        ]
        ifFalse: [
            object2 isReusable
            ifFalse: [
               playerObjects := playerObjects copyWithout: object2.
            ]
        ]."

        self useUp: object1.
        self useUp: object2.

        playerObjects add: recipe getResult.

        Transcript show: 'You got: '.
        Transcript show: recipe getResult getName; cr.
        ^true.
    ]

    talkNPC: personName [
        |npcs n|
        npcs := currentLocation getNpcs.
        n := self findByName: personName and: npcs.

        n = nil
        ifTrue: [
            Transcript show: 'There is no '.
            Transcript show: personName; cr.
            ^false.
        ].

        n talk.
        ^true.
    ]

    findExchangeByObject: needObject and: exchanges [
        exchanges do: [ :exchange | exchange getNeededObject = needObject ifTrue:[
            ^exchange.
        ]].
        ^nil.
    ]

    giveNPC: objectName and: personName [
        | npcs i n newGameObject exchange done |
        done := false.
        npcs := currentLocation getNpcs.
        n := self findByName: personName and: npcs.

        n = nil
        ifTrue: [
            Transcript show: 'There is no '.
            Transcript show: personName; cr.
            ^false.
        ].

        i := self findByName: objectName and: playerObjects.
        i = nil
        ifTrue: [
            Transcript show: 'You don''t have '.
            Transcript show: objectName; cr.
            ^false.
        ].

        exchange := self findExchangeByObject: i and: n getExchanges.

        exchange = nil
        ifTrue: [
            Transcript show: 'He doesn''t want this'; cr.
            ^false.
        ].

        newGameObject := exchange getOfferedObject.
        playerObjects add: newGameObject.
        Transcript show: exchange getDescription; cr.
        Transcript show: 'You got '.
        Transcript show: newGameObject getName; cr.
        playerObjects remove: i.
        ^true.
    ]

    checkNumOfArgs: subs and: num [
        subs size = num
        ifTrue: [
            ^true.
        ]
        ifFalse: [
            Transcript show: 'Wrong number of arguments. Required: '.
            Transcript show: num printString; cr.
            ^false.
        ].
    ]

    leave [
        | object |
        currentLocation getName = 'Ocean'
        ifFalse: [
            Transcript show: 'You are not at the ocean'; cr.
            ^false.
        ].

        object := self findByName: 'raft' and: playerObjects.
        object = nil
        ifTrue: [
            Transcript show: 'You need something to cross the ocean'; cr.
            ^false.
        ].

        Transcript show: 'You left the island by raft. You won!'; cr.
        isGameOver := true.
        ^true.
    ]

    "Run the game."
    run [
        | cmd isUnknown input subs |

        self printIntroduction.
        self printInstructions.
        self printLocation: currentLocation.

        "Game loop."
        [isGameOver] whileFalse: [

            isUnknown := true.
            input := self readCommand.
            subs := input substrings.
            cmd := subs first.

            cmd = 'instructions' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self printInstructions.
                ]
            ].

            cmd = 'n' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self updateLocation: currentLocation getN.
                ]
            ].
            cmd = 'e' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self updateLocation: currentLocation getE.
                ]
            ].
            cmd = 's' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self updateLocation: currentLocation getS.
                ]
            ].
            cmd = 'w' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self updateLocation: currentLocation getW.
                ]
            ].

            cmd = 'inventory' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self printInventory: playerObjects.
                ]
            ].

            cmd = 'look' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self printLocation: currentLocation.
                    Transcript show: ''; cr.
                    self lookAround: currentLocation.
                ]
            ].

            cmd = 'examine' ifTrue: [
                |arg|
                isUnknown := false.
                (self checkNumOfArgs: subs and: 2)
                ifTrue: [
                    arg:= subs at:2.
                    self examine: arg.
                ]
            ].

            cmd = 'take' ifTrue: [
                |arg|
                isUnknown := false.
                (self checkNumOfArgs: subs and: 2)
                ifTrue: [
                    arg:= subs at:2.
                    self pick: arg.
                ]
            ].

            cmd = 'use' ifTrue: [
                |arg it1 it2|
                isUnknown := false.
                (self checkNumOfArgs: subs and: 3)
                ifTrue: [
                    it1 := subs at: 2.
                    it2 := subs at: 3.
                    self use: it1 and: it2.
                ]
            ].

            cmd = 'talk' ifTrue: [
                | personName |
                isUnknown := false.
                (self checkNumOfArgs: subs and: 2)
                ifTrue: [
                    personName := subs at:2.
                    self talkNPC: personName.
                ]
            ].

            cmd = 'give' ifTrue: [
                | objectName personName |
                isUnknown := false.
                (self checkNumOfArgs: subs and: 3)
                ifTrue: [
                    objectName := subs at:2.
                    personName := subs at:3.
                    self giveNPC: objectName and: personName.
                ]
            ].

            cmd = 'leave' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self leave.
                ]
            ].

            cmd = 'halt' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    isGameOver := true.
                ]
            ].

            isUnknown ifTrue: [
                self printLines: #('Unrecognized command.' '')
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
