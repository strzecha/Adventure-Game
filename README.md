# Adventure Game
The project was inspired by the subject "PARP": Warsaw University of Technology

A text-based game written in three different programming languages:
- Prolog (logic)
- Haskell (functional)
- Smalltalk (object-oriented)

Player has to leave an island by collecting various objects, combining them and talking with NPCs.

## Run
To run the program you need install packages:
```
sudo apt-get install -y swi-prolog
sudo apt-get install -y haskell-platform
sudo apt-get install -y gnu-smalltalk
```

### Prolog
Type in terminal:
```
swipl
?- [adventure].
?- start.
```

### Haskell
Type in terminal:
```
ghci
Prelude> :l adventure.hs
*Main> start
```

### Smalltalk
Type in terminal:
```
gst adventure.st
```

## Map + Recipes
![map](https://user-images.githubusercontent.com/75899133/177806217-29c1ac00-2f82-45ba-9a10-1065f5db7692.png)
