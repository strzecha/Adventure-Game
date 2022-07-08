Object subclass: NPC [
    | npcName npcDescription npcSpeech exchanges |

    NPC class >> newNPC: name desc: description speech: speak exchange: exs [
          | n |
          n := super new. 
          n initNPC: name desc: description speech: speak exchange: exs.
          ^n
    ]

    initNPC: name desc: description speech: speak exchange: exs [
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
