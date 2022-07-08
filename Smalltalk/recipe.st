Object subclass: Recipe [
    | object1 object2 result |
    Recipe class >> newRecipe: obj1 gameObject2: obj2 res: obj3 [
        | r |
        r := super new. 
        r initRecipe: obj1 gameObject2: obj2 res: obj3.
        ^r
    ]
    
    initRecipe: obj1 gameObject2: obj2 res: obj3 [
        object1 := obj1.
        object2 := obj2.
        result := obj3.
    ]

    getObject1 [
        ^object1
    ]

    getObject2 [
        ^object2
    ]

    getResult [
        ^result
    ]

]
