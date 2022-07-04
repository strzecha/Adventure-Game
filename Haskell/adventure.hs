import Data.List as List

data GameState = GameState 
    { currentLocation   :: Location
    , gameOver          :: Bool
    , output            :: String
    , playerItems       :: [Item]
    , recipes           :: [Recipe]
    , locationsDiscovered :: [Integer]
    }

data Location = Location
    {
        locationID      :: Integer
    ,   locationName    :: String
    ,   locationDescription     :: String
    ,   north           :: Maybe Location
    ,   west            :: Maybe Location
    ,   south           :: Maybe Location
    ,   east            :: Maybe Location
    ,   locationItems   :: [Item]
    ,   locationNPCs    :: [NPC]
    ,   locationDark    :: Bool
    } deriving (Eq, Show)

data Item = Item
    {
        itemName         :: String
    ,   itemDescription  :: String
    ,   itemPickable     :: Bool
    } deriving (Eq, Show)

data Recipe = Recipe
    {
        recipeItem1     :: Item
    ,   recipeItem2     :: Item
    ,   recipeProduct   :: Item
    } deriving (Eq, Show)

data Exchange = Exchange
    {
        neededItem          :: Item
    ,   offeredItem         :: Item
    ,   exchangeDescription :: String
    } deriving (Eq, Show)

data NPC = NPC
    {
        npcName         :: String
    ,   npcSpeech       :: String
    ,   npcDescription  :: String
    ,   npcExchanges    :: [Exchange]
    } deriving (Eq, Show)

newLocation :: Location
newLocation = Location{
    locationID = -1,
    locationName = "",
    locationDescription = "",
    north = Nothing,
    west = Nothing,
    south = Nothing,
    east = Nothing,
    locationItems = [],
    locationNPCs = [],
    locationDark = False
}

newItem :: Item
newItem = Item{
    itemName = "",
    itemDescription = "",
    itemPickable = False
}

newRecipe :: Recipe
newRecipe = Recipe{
    recipeItem1 = blankItem,
    recipeItem2 = blankItem,
    recipeProduct = blankItem
}

newNPC :: NPC
newNPC = NPC{
    npcName = "",
    npcSpeech = "",
    npcDescription = "",
    npcExchanges = []
}

newExchange :: Exchange
newExchange = Exchange{
    neededItem = blankItem,
    offeredItem = blankItem,
    exchangeDescription = ""
}


introductionText = [
    ""
    ]

instructionsText = [
    "Available commands are:",
    "start                  -- to start the game.",
    "n  s  e  w             -- to go in that direction.", 
    "take Object            -- to pick up an object.",
    "drop Object            -- to put down an object.",
    "examine Object         -- to examine an object.",
    "use Object Object      -- to use the objects together.",
    "inventory              -- to see the objects you are holding.",
    "look                   -- to inspect current location."
    "look_around            -- to see where you can go."
    "talk NPC               -- to talk with NPC.",
    "give Object NPC        -- to give an object to NPC.",
    "instructions           -- to see this message again."
    "halt                   -- to end the game and quit.",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
                  
printIntroduction = printLines introductionText
printInstructions = printLines instructionsText

-- locations


-- items
totem = newItem{itemName="totem"}
tree = newItem{itemName="tree"}
stick = newItem{itemName="stick", itemPickable=True}
bananaTree = newItem{itemName="banana_tree"}
lavaSource = newItem{itemName="lava_source"}
pond = newItem{itemName="pond"}
rose = newItem{itemName="rose", itemPickable=True}
dandelion = newItem{itemName="dandelion", itemPickable=True}
bush = newItem{itemName="bush"}
note1 = newItem{itemName="note1", itemPickable=True}
note2 = newItem{itemName="note2", itemPickable=True}
note3 = newItem{itemName="note3", itemPickable=True}
note4 = newItem{itemName="note4", itemPickable=True}
note5 = newItem{itemName="note5", itemPickable=True}
notebook = newItem{itemName="notebook", itemPickable=True}
palm = newItem{itemName="palm"}
signpost = newItem{itemName="signpost"}
stone = newItem{itemName="stone", itemPickable=True}
well = newItem{itemName="well"}
string = newItem{itemName="string", itemPickable=True}
wreck = newItem{itemName="wreck"}
meat = newItem{itemName="meat", itemPickable=True}
cloth = newItem{itemName="cloth", itemPickable=True}
sheets = newItem{itemName="sheets", itemPickable=True}
blastFurnace = newItem{itemName="blast_furnace"}
anvil = newItem{itemName="anvil"}
banana = newItem{itemName="banana", itemPickable=True}
wood = newItem{itemName="wood", itemPickable=True}
mast = newItem{itemName="mast", itemPickable=True}
brushwood = newItem{itemName="brushwood", itemPickable=True}
torch = newItem{itemName="torch", itemPickable=True}
flamingTorch = newItem{itemName="flaming_torch", itemPickable=True}
rawIron = newItem{itemName="raw_iron", itemPickable=True}
fishingRod = newItem{itemName="fishing_rod", itemPickable=True}
liquidIron = newItem{itemName="liquid_iron", itemPickable=True}
swordForm = newItem{itemName="sword_form", itemPickable=True}
fish = newItem{itemName="fish", itemPickable=True}
hotSword = newItem{itemName="hot_sword", itemPickable=True}
waterBucket = newItem{itemName="water_bucket", itemPickable=True}
sword = newItem{itemName="sword", itemPickable=True}
hardwood = newItem{itemName="hardwood", itemPickable=True}
deck = newItem{itemName="deck", itemPickable=True}
sail = newItem{itemName="sail", itemPickable=True}
raft = newItem{itemName="raft", itemPickable=True}
people = newItem{itemName="people", itemPickable=True}
ax = newItem{itemName="ax", itemPickable=True}
rope = newItem{itemName="rope", itemPickable=True}
aMap = newItem{itemName="map", itemPickable=True}
mysteriousStone = newItem{itemName="mysterious_stone", itemPickable=True}
bucket = newItem{itemName="bucket", itemPickable=True}
pickax = newItem{itemName="pickax", itemPickable=True}
ironOre = newItem{itemName="iron_ore"}

blankItem = newItem

-- recipes
recipeBanana = newRecipe{recipeItem1=stone, recipeItem2=bananaTree, recipeProduct=banana}
recipeWood = newRecipe{recipeItem1=ax, recipeItem2=tree, recipeProduct=wood}
recipeMast = newRecipe{recipeItem1=ax, recipeItem2=palm, recipeProduct=mast}
recipeBrushwood1 = newRecipe{recipeItem1=ax, recipeItem2=bush, recipeProduct=brushwood}
recipeBrushwood2 = newRecipe{recipeItem1=sword, recipeItem2=bush, recipeProduct=brushwood}
recipeTorch1 = newRecipe{recipeItem1=brushwood, recipeItem2=cloth, recipeProduct=torch}
recipeTorch2 = newRecipe{recipeItem1=stick, recipeItem2=cloth, recipeProduct=torch}
recipeFlamingTorch1 = newRecipe{recipeItem1=torch, recipeItem2=lavaSource, recipeProduct=flamingTorch}
recipeFlamingTorch2 = newRecipe{recipeItem1=torch, recipeItem2=blastFurnace, recipeProduct=flamingTorch}
recipeRawIron = newRecipe{recipeItem1=pickax, recipeItem2=ironOre, recipeProduct=rawIron}
recipeFishingRod = newRecipe{recipeItem1=stick, recipeItem2=string, recipeProduct=fishingRod}
recipeLiquidIron = newRecipe{recipeItem1=blastFurnace, recipeItem2=rawIron, recipeProduct=liquidIron}
recipeSwordForm = newRecipe{recipeItem1=liquidIron, recipeItem2=stick, recipeProduct=swordForm}
recipeFish = newRecipe{recipeItem1=pond, recipeItem2=fishing_rod, recipeProduct=fish}
recipeHotSword = newRecipe{recipeItem1=swordForm, recipeItem2=anvil, recipeProduct=hotSword}
recipeWaterBucket1 = newRecipe{recipeItem1=pond, recipeItem2=bucket, recipeProduct=waterBucket}
recipeWaterBucket2 = newRecipe{recipeItem1=well, recipeItem2=bucket, recipeProduct=waterBucket}
recipeSword1 = newRecipe{recipeItem1=hotSword, recipeItem2=waterBucket, recipeProduct=sword}
recipeSword2 = newRecipe{recipeItem1=pond, recipeItem2=hotSword, recipeProduct=sword}
recipeHardwood = newRecipe{recipeItem1=wood, recipeItem2=wood, recipeProduct=hardwood}
recipeDeck = newRecipe{recipeItem1=hardwood, recipeItem2=rope, recipeProduct=deck}
recipeSail = newRecipe{recipeItem1=sheets, recipeItem2=mast, recipeProduct=sail}
recipeRaft = newRecipe{recipeItem1=deck, recipeItem2=sail, recipeProduct=raft}
recipePeople = newRecipe{recipeItem1=totem, recipeItem2=mysteriousStone, recipeProduct=people}

-- exchanges
exchangeAx = newExchange{neededItem=phone, offeredItem=thing, exchangeDescription="Have my ax"}

-- NPCs
native = newNPC{npcName="native", npcDescription="nat", npcSpeech="Hello", npcExchanges=[exchangeAx]}

-- describing
join :: [Item] -> String
join [] = ""
join [item] = itemName item
join (item:items) = (itemName item) ++ ", " ++ (join items)

joinNPC :: [NPC] -> String
joinNPC [] = ""
joinNPC [npc] = npcName npc
joinNPC (npc:npcs) = (npcName npc) ++ ", " ++ (joinNPC npcs)

printMessage :: String -> GameState -> GameState
printMessage msg state = state{output=msg}

describeSituation :: GameState -> IO ()
describeSituation state = printLines [locationName location, (output state), "",
                                      (desc), ""]
    where
        location = currentLocation state
        desc = 
            if isVisible location (playerItems state) then
                description (locationItems (currentLocation state)) (locationNPCs (currentLocation state))
            else 
                "You see darkness only"     

isVisible :: Location -> [Item] -> Bool
isVisible location inventory = 
    if locationDark location && not (elem flamingTorch inventory) then
        False
    else
        True

description :: [Item] -> [NPC] -> String
description items npcs = "There are: " ++ (join items) ++ ", " ++ (joinNPC npcs)

showInventory :: GameState -> GameState
showInventory state = printMessage (descItems) state
    where
        descItems = join (playerItems state)


-- moving
getLocation :: Location -> String -> Maybe (Maybe Location)
getLocation location direction = do
    case direction of
        "n" -> return (north location)
        "s" -> return (south location)
        "w" -> return (west location)
        "e" -> return (east location)

canMove :: Location -> String -> Maybe Bool
canMove location direction = do
    way <- getLocation location direction
    case way of
        Nothing -> return False
        Just location -> return True

moveTo :: Location -> GameState -> GameState
moveTo location state = state{currentLocation=location, output="",
                            locationsDiscovered=newIDs}
    where
        id = locationID location
        locationsID = locationsDiscovered state
        newIDs = 
            if elem aMap (playerItems state) then
                id:locationsID
            else
                locationsID
        


move :: String -> GameState -> GameState
move direction state = go (nextLoc) state
    where
        Just nextLoc = getLocation (currentLocation state) direction

        go location = case location of
            Nothing -> printMessage "You can't go that way"
            Just location -> moveTo location

getNameLocation :: Location -> GameState -> String
getNameLocation location state | not (elem id (locationsDiscovered state)) = "undiscovered"
                               | otherwise = locationName location
    where
        id = locationID location

lookAround :: GameState -> GameState
lookAround state = state{output=desc}
    where
        nLoc = north (currentLocation state)
        eLoc = east (currentLocation state)
        sLoc = south (currentLocation state)
        wLoc = west (currentLocation state)
        descN =
            case nLoc of
                Nothing -> ""
                Just nLoc -> "s -> " ++ (getNameLocation nLoc state) ++ "\n"
        descE =
            case eLoc of
                Nothing -> ""
                Just eLoc -> "e -> " ++ (getNameLocation eLoc state) ++ "\n"
        descS =
            case sLoc of
                Nothing -> ""
                Just sLoc -> "s -> " ++ (getNameLocation sLoc state) ++ "\n"
        descW =
            case wLoc of
                Nothing -> ""
                Just wLoc -> "w -> " ++ (getNameLocation wLoc state) ++ "\n"
        
        desc = descN ++ descE ++ descS ++ descW

exchangeInList :: Item -> [Exchange] -> Maybe Exchange
exchangeInList item [] = Nothing
exchangeInList item (exchange:exchanges) = do
    if item == neededItem exchange then
        Just exchange
    else
        exchangeInList item exchanges

npcInList :: String -> [NPC] -> Maybe NPC
npcInList npName [] = Nothing
npcInList npName (npc:npcs) = do
    if npName == name then
        Just npc
    else 
        npcInList npName npcs
    where
        name = (npcName npc)

talk :: String -> GameState -> GameState
talk npcName state = printMessage (message npc) state
    where
        npc = npcInList npcName (locationNPCs (currentLocation state))
        message npc = 
            case npc of
                Just npc -> npcSpeech npc
                Nothing -> "There is no " ++ npcName

examineNPC :: String -> GameState -> GameState
examineNPC npcName state = printMessage (message npc) state
    where
        npc = npcInList npcName (locationNPCs (currentLocation state))
        message npc = 
            case npc of
                Just npc -> npcDescription npc
                Nothing -> "There is no " ++ npcName

give :: String -> String -> GameState -> GameState
give itName npName state = prepareExchange item npc state
    where
        npc = npcInList npName (locationNPCs (currentLocation state))
        item = itemInList itName (playerItems state)

        prepareExchange :: Maybe Item -> Maybe NPC -> GameState -> GameState
        prepareExchange item npc state = 
            case item of
                Nothing -> printMessage ("You don't have "++itName) state
                Just item -> 
                    case npc of
                        Nothing -> printMessage ("There is no "++npName) state
                        Just npc -> tryExchange (exchangeInList item (npcExchanges npc)) state

tryExchange :: Maybe Exchange -> GameState -> GameState
tryExchange exchange state =
    case exchange of
        Nothing -> printMessage "He don't want it" state
        Just exchange -> doExchange exchange state

doExchange :: Exchange -> GameState -> GameState
doExchange exchange state = state{output=desc++"\nYou got "++(itemName gift), 
                                    playerItems=gift:inventory}
    where
        desc = exchangeDescription exchange
        gift = offeredItem exchange
        inventory = playerItems state 

dropItem :: String -> GameState -> GameState
dropItem itName state = tryDrop item state
    where
        item = itemInList itName (playerItems state)

        tryDrop :: Maybe Item -> GameState -> GameState
        tryDrop item state = 
            case item of
                Nothing -> printMessage ("You don't have "++itName) state
                Just item -> state{output="You dropped "++itName, 
                                    playerItems=removeItem item (playerItems state),
                                    currentLocation=(currentLocation state){locationItems=item:(locationItems (currentLocation state))}
                                    }

-- interact with items
examine :: String -> GameState -> GameState
examine itName state = printMessage (message item) state
    where 
        item = itemInList itName ((locationItems (currentLocation state))++(playerItems state))

        message :: Maybe Item -> String
        message it = 
            case it of
                Just it -> 
                    if (itemDescription it) /= "" then
                        itemDescription it
                    else
                        "You don't know anything about "++(itemName it)
                Nothing -> "There is no "++itName

findRecipe :: Item -> Item -> [Recipe] -> Maybe Recipe
findRecipe _ _ [] = Nothing
findRecipe item1 item2 (recipe:recipes) = 
    if (recipeItem1 recipe) == item1 && (recipeItem2 recipe) == item2 || 
       (recipeItem2 recipe) == item1 && (recipeItem1 recipe) == item2 then
        Just recipe
    else
        findRecipe item1 item2 recipes 

craftItem :: Item -> Item -> Recipe -> GameState -> GameState
craftItem item1 item2 recipe state = state{playerItems=item:inventory, currentLocation=location{locationItems=locItems},
                                           output="You got "++(itemName item)}
    where
        location = (currentLocation state)

        inventory = if elem item2 (playerItems state) then
                        removeItem item1 (removeItem item2 (playerItems state))
                    else
                        removeItem item1 (playerItems state)

        locItems =  if elem item2 (playerItems state) then
                        (locationItems location)
                    else
                        removeItem item2 (locationItems location)
        item = (recipeProduct recipe)

checkRecipe :: Item -> Item -> GameState -> GameState
checkRecipe item1 item2 state = 
    case recipe of
        Nothing -> printMessage "You can't use it that way" state
        Just recipe -> craftItem item1 item2 recipe state
    where
        recipe = findRecipe item1 item2 (recipes state)

use :: String -> String -> GameState -> GameState
use itemName1 itemName2 state = tryCraft item1 item2 state
    where
        item1 = itemInList itemName1 (playerItems state)
        itemLoc = itemInList itemName2 (locationItems (currentLocation state))
        itemInv = itemInList itemName2 (playerItems state)
        item2 = if itemInv /= Nothing then itemInv else itemLoc

        tryCraft :: Maybe Item -> Maybe Item -> GameState -> GameState
        tryCraft item1 item2 = do
            case item1 of
                Nothing -> printMessage ("You don't have "++itemName1)
                Just item1 -> case item2 of
                                Nothing -> printMessage ("You don't have "++itemName2)
                                Just item2 -> checkRecipe item1 item2

itemInList :: String -> [Item] -> Maybe Item
itemInList itName [] = Nothing
itemInList itName (item:items) = do
    if itName == name then
        Just item
    else 
        itemInList itName items
    where
        name = (itemName item)

pickUp :: String -> GameState -> GameState
pickUp itemName state = tryPut item state
    where
    item = itemInList itemName (locationItems (currentLocation state))
    tryPut it =
        case it of
            Nothing -> printMessage ("There is no "++itemName)
            Just it -> if itemPickable it then 
                            putInventory it 
                       else 
                            printMessage "You can't do this"

removeItem :: Item -> [Item] -> [Item]
removeItem item list = List.delete item list


putInventory :: Item -> GameState -> GameState
putInventory item state = state{currentLocation=location{locationItems=items}, 
                                playerItems=item:inventory, 
                                output="You picked up " ++ (itemName item)}
    where 
        inventory = playerItems state
        location = currentLocation state
        items = removeItem item (locationItems location)

-- commands
readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs

quit :: GameState -> GameState
quit state = state{gameOver=True}

parseCommand :: String -> GameState -> GameState
parseCommand input = 
    case head (words input) of
        "n" -> move input
        "s" -> move input
        "w" -> move input
        "e" -> move input
        "examine" -> examine ((words input)!!1)
        "use" -> use ((words input)!!1) ((words input)!!2)
        "inventory" -> showInventory 
        "pick" -> pickUp ((words input)!!1)
        "talk" -> talk ((words input)!!1)
        "examineNPC" -> examineNPC ((words input)!!1)
        "give" -> give ((words input)!!1) ((words input)!!2)
        "look_around" -> lookAround
        "drop" -> dropItem ((words input)!!1)
        "quit" -> quit
        otherwise -> (\state -> state{output="Unrecognized command"})

gameLoop :: IO ()
gameLoop = do
    state <- startingState
    finalState <- play state
    return ()
    where
        startingState = do
            return (GameState someplace False "" [phone, pen] [recipeBanana, recipeSign] [0])
        play state = do
            newState <- playFrame state
            if gameOver newState then
                return newState
            else
                play newState
        playFrame state = do
            describeSituation state
            command <- promptForCommand state
            return $ command state
        
        promptForCommand state = do
            input <- readCommand
            return (parseCommand input)

start :: IO ()
start = do
    printIntroduction
    printInstructions
    gameLoop
