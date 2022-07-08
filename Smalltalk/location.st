Object subclass: Location [
    | locationId locationName locationDescription north south west east |
    | locationObjects locationNPCs locationDark locationDiscovered |

    Location class >> newLocation: id newName: name desc: description northLoc: n eastLoc: e southLoc: s westLoc: w locGameObjects: objects locNPCs: npcs isDark: dark [
        | l |
        l := super new. 
        l initLocation: id newName: name desc: description northLoc: n eastLoc: e southLoc: s westLoc: w locGameObjects: objects locNPCs: npcs isDark: dark.
        ^l
    ]

    initLocation: id newName: name desc: description northLoc: n eastLoc: e southLoc: s westLoc: w locGameObjects: objects locNPCs: npcs isDark: dark [
        locationId := id.
        locationName := name.
        locationDescription := description.
        north := n.
        west := w.
        south := s.
        east := e.
        locationObjects := OrderedCollection new.
        objects do: [ :object | locationObjects add: object ].
        locationNPCs := npcs.
        locationDark := dark.
        locationDiscovered := false.
    ]

    printOn: stream [
        super printOn: stream.
        locationName printOn: stream.
        locationDescription printOn: stream.
        locationObjects printOn: stream.
        locationNPCs printOn: stream.
    ]

    getN [
        ^north
    ]
    
    getE [
        ^east
    ]

    getS[
        ^south
    ]
    
    getW [
        ^west
    ]

    getId [
        ^locationId
    ]
    
    getObjects [
    ^locationObjects
    ]

    getName [
    ^locationName
    ]

    getDescription [
        ^locationDescription
    ]

    getNPCs [
    ^locationNPCs
    ]

    removeObject: object [
    locationObjects remove: object.
    ]

    addObject: object [
    locationObjects add: object.
    ]

    isDark [
        ^locationDark
    ]

    isDiscovered [
        ^locationDiscovered
    ]

    discover [
        locationDiscovered := true.
    ]
]
