Object subclass: Npc [
    |npcName npcDescrption speakDialogue npcGameObject npcNeedsGameObject|

    Npc class >> newNpc: name desc: description dialogue: speak npcIt: it needsIt: needit [
          | n |
          n := super new. 
          n initNpc: name desc: description dialogue: speak npcIt: it needsIt: needit.
          ^n
    ]

    initNpc: name desc: description dialogue: speak npcIt: it needsIt: needit [
        npcName := name.
        npcDescrption := description.
        speakDialogue := speak.
        npcGameObject := it.
        npcNeedsGameObject := needit.
    ]

    printOn: stream [
        super printOn: stream.
        npcName printOn: stream.
        npcDescrption printOn: stream.
    ]

    getName [
        ^npcName
    ]

    talk [
        Transcript show: speakDialogue; cr.
    ]

    getNeedGameObject [
        ^npcNeedsGameObject
    ]

    getGameObject [
        ^npcGameObject
    ]

    setGameObject: objectName [
       npcGameObject := objectName.
    ]
]
