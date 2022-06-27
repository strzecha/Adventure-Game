data GameState = GameState 
    { currentLocation   :: Location
    , gameOver          :: Bool
    , output            :: String
    , playerItems       :: [Item]
    , recipes           :: [Recipe]
    }

data Location = Location
    {
        locationName    :: String
    ,   locationDescription     :: String
    ,   north           :: Maybe Location
    ,   west            :: Maybe Location
    ,   south           :: Maybe Location
    ,   east            :: Maybe Location
    ,   locationItems   :: [Item]
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
    }

newLocation :: Location
newLocation = Location{
    locationName = "",
    locationDescription = "",
    north = Nothing,
    west = Nothing,
    south = Nothing,
    east = Nothing,
    locationItems =[]
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


introductionText = [
    ""
    ]

instructionsText = [
    "Available commands are:",
    "",
    "n, w, s, e    -- move to north, west, south, east", 
    "pick item     -- pick up item",
    "examine item  -- examine item",
    "inventory     -- show inventory",  
    "quit          -- to end the game and quit.",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
                  
printIntroduction = printLines introductionText
printInstructions = printLines instructionsText

-- locations
someplace = newLocation{locationName="Someplace", locationDescription="Desc Someplace",
                        north=(Just beach2), west=(Just jungle2), south=(Just jungle3), east=(Just jungle1),
                        locationItems=[pen, sign, thing]}
jungle1 = newLocation{locationName="Jungle1", locationDescription="Desc Jungle",
                    west=(Just someplace)}
jungle2 = newLocation{locationName="Jungle2", locationDescription="Desc Jungle",
                    east=(Just someplace)}
jungle3 = newLocation{locationName="Jungle3", locationDescription="Desc Jungle",
                      north=(Just someplace)}
beach2 = newLocation{locationName="Beach2", locationDescription="Desc beach",
                     south=(Just someplace)}

-- items
phone = newItem{itemName="phone", itemDescription="Glowing", itemPickable=True}
pen = newItem{itemName="pen", itemDescription="Shiny", itemPickable=True}
thing = newItem{itemName="thing", itemDescription="Strange", itemPickable=True}
sign = newItem{itemName="sign", itemDescription="W - left, E - right"}

blankItem = newItem

-- recipes
recipeBanana = newRecipe{recipeItem1=phone, recipeItem2=pen, recipeProduct=thing}

-- describing
join :: [Item] -> String
join [] = "nothing"
join [item] = itemName item
join (item:items) = (itemName item) ++ ", " ++ (join items)

printMessage :: String -> GameState -> GameState
printMessage msg state = state{output=msg}

describeSituation :: GameState -> IO ()
describeSituation state = printLines [(locationName (currentLocation state)), (output state), "",
                                      (descItems), ""]
    where
        descItems = descriptionItems (locationItems (currentLocation state))

descriptionItems :: [Item] -> String
descriptionItems items = "You see: " ++ (join items)

showInventory :: GameState -> GameState
showInventory state = printMessage (descItems) state
    where
        descItems = "You have: " ++ join (playerItems state)


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
moveTo location state = state{currentLocation=location, output=""}

move :: String -> GameState -> GameState
move direction state = go (nextLoc) state
    where
        Just nextLoc = getLocation (currentLocation state) direction

        go location = case location of
            Nothing -> printMessage "You can't go that way"
            Just location -> moveTo location

-- interact with items
examine :: String -> GameState -> GameState
examine itName state = printMessage (message item) state
    where 
        item = itemInList itName ((locationItems (currentLocation state))++(playerItems state))
        message it = 
            case it of
                Just it -> itemDescription it
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
craftItem item1 item2 recipe state = state{playerItems=item:inventory, output="You got "++(itemName item)}
    where
        inventory = removeItem item2 (removeItem item1 (playerItems state))
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
        item2 = itemInList itemName2 ((locationItems (currentLocation state))++(playerItems state))

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
removeItem _ [] = []
removeItem it (item:items) | it == item = removeItem it items
                    | otherwise = item : removeItem it items

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
        "quit" -> quit
        otherwise -> (\state -> state{output="Unrecognized command"})

gameLoop :: IO ()
gameLoop = do
    state <- startingState
    finalState <- play state
    return ()
    where
        startingState = do
            return (GameState someplace False "" [phone, pen] [recipeBanana])
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
