Object subclass: Npc [
    | npcName npcDescription npcSpeech exchanges |

    Npc class >> newNpc: name desc: description speech: speak exchange: exs [
          | n |
          n := super new. 
          n initNpc: name desc: description speech: speak exchange: exs.
          ^n
    ]

    initNpc: name desc: description speech: speak exchange: exs [
        npcName := name.
        npcDescription := description.
        npcSpeech := speak.
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

    getDescription [
        ^npcDescription
    ]

    talk [
        Transcript show: npcSpeech; cr.
    ]

    getExchanges [
        ^exchanges
    ]
]
