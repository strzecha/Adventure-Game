Object subclass: Location [
     |locationId locationName locationDescrption north south west east |
     |locationGameObjects |
     |locationNpcs |
     |locationDark |
     |locationDiscovered |

     Location class >> newLocation: id newName: name desc: description northLoc: lNorth westLoc: lWest southLoc: lSouth eastLoc: lEast locGameObjects: objects locNpcs: npcs isDark: dark isDiscovered: discover [
          | l |
          l := super new. 
          l initLocation: id newName: name desc: description northLoc: lNorth westLoc: lWest southLoc: lSouth eastLoc: lEast locGameObjects: objects locNpcs: npcs isDark: dark isDiscovered: discover.
          ^l
     ]

     initLocation: id newName: name desc: description northLoc: lNorth westLoc: lWest southLoc: lSouth eastLoc: lEast locGameObjects: objects locNpcs: npcs isDark: dark isDiscovered: discover [
          locationId := id.
          locationName := name.
          locationDescrption := description.
          north := lNorth.
          west := lWest.
          south := lSouth.
          east := lEast.
          locationGameObjects := objects.
          locationNpcs := npcs.
          locationDark := dark.
          locationDiscovered := discover.
     ]

     printOn: stream [
          super printOn: stream.
          locationName printOn: stream.
          locationDescrption printOn: stream.
          locationGameObjects printOn: stream.
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
     
     getGameObjects [
        ^locationGameObjects
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
        locationGameObjects remove: name.
     ]

     removeGameObject: object [
          locationGameObjects := locationGameObjects copyWithout: object.
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
