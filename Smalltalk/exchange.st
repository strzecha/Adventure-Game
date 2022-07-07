Object subclass: Npc [
    |npcName npcDescrption speakDialogue talkable npcGameObject npcNeedsGameObject|

    Npc class >> newNpc: name desc: description dialogue: speak isTalkable: talk npcIt: it needsIt: needit [
          | n |
          n := super new. 
          n initNpc: name desc: description dialogue: speak isTalkable: talk npcIt: it needsIt: needit.
          ^n
    ]

    initNpc: name desc: description dialogue: speak isTalkable: talk npcIt: it needsIt: needit [
        npcName:=name.
        npcDescrption:=description.
        speakDialogue:=speak.
        talkable:=talk.
        npcGameObject:=it.
        npcNeedsGameObject:=needit.
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
        talkable 
        ifTrue: [
            Transcript show: speakDialogue; cr
            ]
        ifFalse: [ 
            Transcript show: npcName.
            Transcript show: ' is not talkable.'; cr.
            ].
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
