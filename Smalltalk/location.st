Object subclass: Location [
     |locationId locationName locationDescrption north south west east |
     |locationObjects |
     |locationNpcs |
     |locationDark |
     |locationDiscovered |

     Location class >> newLocation: id newName: name desc: description northLoc: lNorth westLoc: lWest southLoc: lSouth eastLoc: lEast locGameObjects: objects locNpcs: npcs isDark: dark [
          | l |
          l := super new. 
          l initLocation: id newName: name desc: description northLoc: lNorth westLoc: lWest southLoc: lSouth eastLoc: lEast locGameObjects: objects locNpcs: npcs isDark: dark.
          ^l
     ]

     initLocation: id newName: name desc: description northLoc: lNorth westLoc: lWest southLoc: lSouth eastLoc: lEast locGameObjects: objects locNpcs: npcs isDark: dark [
          locationId := id.
          locationName := name.
          locationDescrption := description.
          north := lNorth.
          west := lWest.
          south := lSouth.
          east := lEast.
          locationObjects := objects.
          locationNpcs := npcs.
          locationDark := dark.
          locationDiscovered := false.
     ]

     printOn: stream [
          super printOn: stream.
          locationName printOn: stream.
          locationDescrption printOn: stream.
          locationObjects printOn: stream.
          locationNpcs printOn: stream.
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
          ^locationDescrption
     ]

     getNpcs [
        ^locationNpcs
     ]

     removeGameObjectByName: name [
        locationObjects remove: name.
     ]

     removeGameObject: object [
          locationObjects := locationObjects copyWithout: object.
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
