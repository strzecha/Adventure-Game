Object subclass: Exchange [
    | neededObject offeredObject description |

    Exchange class >> newExchange: need offerObject: offer descr: d [
          | ex |
          ex := super new. 
          ex initExchange: need offerObject: offer descr: d.
          ^ex
    ]

    initExchange: need offerObject: offer descr: d [
        neededObject := need.
        offeredObject := offer.
        description := d.
    ]

    getOfferedObject [
        ^offeredObject
    ]

    getNeededObject [
        ^neededObject
    ]

    getDescription [
       ^description
    ]
]
