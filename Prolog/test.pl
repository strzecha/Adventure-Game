usable(dupa, jasia, kalosz).

use(X, Y) :- 
    (usable(X, Y, R);usable(Y, X, R)),
    write(R), !.