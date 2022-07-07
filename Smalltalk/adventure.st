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
        Transcript cr.
        Transcript show: '> '.
        ^FileStream stdin nextLine
    ]

    printNames: objects [
        objects do: [ :object | Transcript show: object getName.
                                Transcript show: ', '. ].
        Transcript cr.
    ]

    printInventory[
        self printNames: playerObjects.
    ]

    printLocation: location [
        location isDark & ((self findByName: 'flamingTorch' and: playerObjects) = nil) ifTrue: [
            Transcript show: 'You see darkness only'; cr.
            ^false.
        ].
        Transcript show: location getDescription; cr.
        Transcript show: 'There are: '.
        self printNames: location getObjects, location getNpcs.
        ^true.
    ]

    printGameObjectDescription: object [
        object getDescription = ''
        ifTrue: [
            Transcript show: 'You don''t know anything about ', object getName; cr.
            ^false.
        ].
        Transcript show: object getDescription; cr.
        ^true.
    ]

    examine: objectName [
        (playerObjects, currentLocation getObjects, currentLocation getNpcs) do: [:object | object getName = objectName ifTrue: [
            self printGameObjectDescription: object. 
            ^true
        ]].
        Transcript show: 'There is no ', objectName; cr.
        ^false.
    ]

    findByName: name and: anArray [
        anArray do: [:object | object getName = name ifTrue: [ ^object ]].
        ^nil.
    ]

    findLocationByIndex: index [
        locations do: [:location | location getId = index ifTrue: [ ^location ]].
        ^nil.
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
        north := self findLocationByIndex: location getN.
        south := self findLocationByIndex: location getS.
        east := self findLocationByIndex: location getE.
        west := self findLocationByIndex: location getW.

        self printDirection: north and: 'n'.
        self printDirection: east and: 'e'.
        self printDirection: west and: 'w'.
        self printDirection: south and: 's'.
    ]
    
    updateLocation: locationIndex [
        | newLocation |
        newLocation := self findLocationByIndex: locationIndex.
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
        self printLocation: currentLocation.
        ^true.
    ]

    pick: objectName [
        | its i |
        its := currentLocation getObjects.
        i := self findByName: objectName and: its.
        i = nil
        ifTrue: [
            Transcript show: 'There is no ', objectName; cr.
            ^false.
        ].

        i isPickable = true 
        ifFalse: [
            Transcript show: 'You can''t do this.'; cr.
            ^false.
        ].
        playerObjects add: i.
        currentLocation removeObject: i.
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
        (recipe getObject1 = object2 & recipe getObject1 = object2)) ifTrue: [
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
        |object1 object2 recipe result objectLoc|

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

    talkNPC: personName [
        |npcs n|
        npcs := currentLocation getNpcs.
        n := self findByName: personName and: npcs.

        n = nil
        ifTrue: [
            Transcript show: 'There is no ', personName; cr.
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
            Transcript show: 'There is no ', personName; cr.
            ^false.
        ].

        i := self findByName: objectName and: playerObjects.
        i = nil
        ifTrue: [
            Transcript show: 'You don''t have ', objectName; cr.
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
        Transcript show: 'You got ', newGameObject getName; cr.
        playerObjects remove: i.
        ^true.
    ]

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

    leave [
        | object |
        currentLocation getName = 'Ocean'
        ifFalse: [
            Transcript show: 'You aren''t at the ocea'; cr.
            ^false.
        ].

        object := self findByName: 'raft' and: playerObjects.
        object = nil
        ifTrue: [
            Transcript show: 'You can''t swim across the ocean. You need boat.'; cr.
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
                    self printInventory.
                ]
            ].

            cmd = 'look' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
                    self printLocation: currentLocation.
                ]
            ].

            cmd = 'look_around' ifTrue: [
                isUnknown := false.
                (self checkNumOfArgs: subs and: 1)
                ifTrue: [
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

            cmd = 'drop' ifTrue: [
                |arg|
                isUnknown := false.
                (self checkNumOfArgs: subs and: 2)
                ifTrue: [
                    arg:= subs at:2.
                    self drop: arg.
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
