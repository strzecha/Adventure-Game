Object subclass: GameObject [
     |objectName objectDescription objectPickable objectReusable|
     
     GameObject class >> newGameObject: name desc: description pick: pickable use: usable [
          | i |
          i := super new. 
          i initGameObject: name desc: description pick: pickable use: usable.
          ^i
     ]
     
     
     initGameObject: name desc: description pick: pickable use: usable [
        objectName := name.
        objectDescription := description.
        objectPickable := pickable.
        objectReusable := usable.
     ]

     printOn: stream [
          super printOn: stream.
          objectName printOn: stream.
          objectDescription printOn: stream.
     ]

     getName [
        ^objectName
     ]

     getDescription [
          ^objectDescription
     ]

     isPickable [
          ^objectPickable
     ]  

     isReusable [
          ^objectReusable
     ]

]
