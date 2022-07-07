Object subclass: Npc [
    | npcName npcDescription speakDialogue exchanges |

    Npc class >> newNpc: name desc: description dialogue: speak exchange: exs [
          | n |
          n := super new. 
          n initNpc: name desc: description dialogue: speak exchange: exs.
          ^n
    ]

    initNpc: name desc: description dialogue: speak exchange: exs [
        npcName := name.
        npcDescription := description.
        speakDialogue := speak.
        exchanges := exs.
    ]

    printOn: stream [
        super printOn: stream.
        npcName printOn: stream.
        npcDescription printOn: stream.
    ]

    getName [
        ^npcName
    ]

    talk [
        Transcript show: speakDialogue; cr.
    ]

    getExchanges [
        ^exchanges
    ]
]
