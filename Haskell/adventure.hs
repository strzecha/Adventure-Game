import Data.List as List

-- custom datatypes
data GameState = GameState 
    { currentLocation       :: Location
    , gameOver              :: Bool
    , output                :: String
    , playerGameObjects           :: [GameObject]
    , recipes               :: [Recipe]
    , discoveredLocationsID :: [Integer]
    , visitedLocations   :: [Location]
    }

data Location = Location
    {
        locationID          :: Integer
    ,   locationName        :: String
    ,   locationDescription :: String
    ,   north               :: Maybe Location
    ,   west                :: Maybe Location
    ,   south               :: Maybe Location
    ,   east                :: Maybe Location
    ,   locationGameObjects       :: [GameObject]
    ,   locationNPCs        :: [NPC]
    ,   locationDark        :: Bool
    } deriving (Eq, Show)

data GameObject = GameObject
    {
        objectName            :: String
    ,   objectDescription     :: String
    ,   objectPickable        :: Bool
    ,   objectReusable        :: Bool
    } deriving (Eq, Show)

data Recipe = Recipe
    {
        recipeGameObject1         :: GameObject
    ,   recipeGameObject2         :: GameObject
    ,   recipeProduct       :: GameObject
    } deriving (Eq, Show)

data Exchange = Exchange
    {
        neededGameObject          :: GameObject
    ,   offeredGameObject         :: GameObject
    ,   exchangeDescription :: String
    } deriving (Eq, Show)

data NPC = NPC
    {
        npcName             :: String
    ,   npcSpeech           :: String
    ,   npcDescription      :: String
    ,   npcExchanges        :: [Exchange]
    } deriving (Eq, Show)

-- custom constructors
newLocation :: Location
newLocation = Location{
    locationID = -1,
    locationName = "",
    locationDescription = "",
    north = Nothing,
    west = Nothing,
    south = Nothing,
    east = Nothing,
    locationGameObjects = [],
    locationNPCs = [],
    locationDark = False
}

newGameObject :: GameObject
newGameObject = GameObject{
    objectName = "",
    objectDescription = "",
    objectPickable = False,
    objectReusable = False
}

newRecipe :: Recipe
newRecipe = Recipe{
    recipeGameObject1 = blankGameObject,
    recipeGameObject2 = blankGameObject,
    recipeProduct = blankGameObject
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
    neededGameObject = blankGameObject,
    offeredGameObject = blankGameObject,
    exchangeDescription = ""
}

-- texts
introductionText = [
    ""
    ]

instructionsText = [
    "Available commands are:",
    "n  s  e  w             -- to go in that direction.", 
    "take Object            -- to pick up an object.",
    "drop Object            -- to put down an object.",
    "examine Object/NPC     -- to examine an object or NPC.",
    "use Object Object      -- to use the objects together.",
    "inventory              -- to see the objects you are holding.",
    "look                   -- to inspect current location.",
    "look_around            -- to see where you can go.",
    "talk NPC               -- to talk with NPC.",
    "give Object NPC        -- to give an object to NPC.",
    "instructions           -- to see these instructions.",
    "leave                  -- to leave an island.",
    "halt                   -- to end the game and quit.",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
                  
printIntroduction = printLines introductionText
printInstructions = printLines instructionsText

-- locations
meadow1 = newLocation{locationID=1, locationName="meadow1", locationDescription="You are in a meadow with a huge wooden totem in the middle.", 
                        locationGameObjects=[totem], east=Just forest1, south=Just forest2}
forest1 = newLocation{locationID=2, locationName="forest1", locationDescription="You are in the forest.",
                        locationGameObjects=[tree, tree, stick], east=Just darkJungle1, south=Just meadow2, west=Just meadow1}
darkJungle1 = newLocation{locationID=3, locationName="dark_jungle1", locationDescription="You entered the very dense jungle.", 
                        locationGameObjects=[bananaTree], east=Just tunnelEntrance, south=Just jungle1, west=Just forest1, locationDark=True}
tunnelEntrance = newLocation{locationID=4, locationName="tunnel_entrance", locationDescription="You entered a dark tunnel.",
                        south=Just tunnel1, west=Just darkJungle1, locationDark=True}
ancientRuin = newLocation{locationID=5, locationName="ancient_ruin", locationDescription="You are in a stone crypt. You can see strange signs on the walls, and in the middle - a rock statue of a knight.",
                        locationNPCs=[ancientGuard], south=Just tunnel2, locationDark=True}
volcanoPeak = newLocation{locationID=6, locationName="volcano_peak", locationDescription="You reached the top. But it is not the top of the mountain. This is the top of the volcano!",
                        locationGameObjects=[lavaSource], south=Just mountainPath1}
forest2 = newLocation{locationID=7, locationName="forest2", locationDescription="You are in the forest.",
                        locationGameObjects=[tree, tree], north=Just meadow1, east=Just meadow2, south=Just fields1}
meadow2 = newLocation{locationID=8, locationName="meadow2", locationDescription="You see a pond in the middle of a flower meadow.", 
                        locationGameObjects=[pond, rose, dandelion], north=Just forest1, east=Just jungle1, west=Just forest2}
jungle1 = newLocation{locationID=9, locationName="jungle1", locationDescription="You are in the jungle.", 
                        locationGameObjects=[bush, tree], locationNPCs=[monkey],
                        north=Just darkJungle1, south=Just jungle2, west=Just meadow2}
tunnel1 = newLocation{locationID=10, locationName="tunnel1", locationDescription="You are in a long rock corridor. You don't see much.",
                        locationGameObjects=[note3], north=Just tunnelEntrance, east=Just tunnel2, locationDark=True}
tunnel2 = newLocation{locationID=11, locationName="tunnel2", locationDescription="You reached a fork. The walls of the tunnel have a strange color...",
                        locationGameObjects=[ironOre], north=Just ancientRuin, south=Just cave, west=Just tunnel1, locationDark=True}
mountainPath1 = newLocation{locationID=12, locationName="mountain_path1", locationDescription="You are getting closer to the top of the mountain. It's getting hot...",
                        locationGameObjects=[note2], north=Just volcanoPeak, south=Just mountainPath2}
fields1 = newLocation{locationID=13, locationName="fields1", locationDescription="You are in the fields.", 
                        locationGameObjects=[note4], north=Just forest1, south=Just beach1}
planeWreck = newLocation{locationID=14, locationName="plane_wreck", locationDescription="You entered the plane wreck. Certainly, more than one person flew on it, unfortunately you do not see anyone.",
                        locationGameObjects=[notebook], south=Just fields2}
jungle2 = newLocation{locationID=15, locationName="jungle2", locationDescription="You are in the jungle.", locationGameObjects=[bush],
                        north=Just jungle1, east=Just darkJungle2, south=Just crossroads}
darkJungle2 = newLocation{locationID=16, locationName="dark_jungle2", locationDescription="You entered the very dense jungle.",
                        locationGameObjects=[bush, stick], south=Just path1, west=Just jungle2, locationDark=True}
cave = newLocation{locationID=17, locationName="cave", locationDescription="You have entered a dark cave. You are clearly not alone here...",
                        locationNPCs=[oldNative], north=Just tunnel2, locationDark=True}
mountainPath2 = newLocation{locationID=18, locationName="mountain_path2", locationDescription="You start climbing the mountain path.",
                        north=Just mountainPath1, south=Just path2}
ocean1 = newLocation{locationID=19, locationName="ocean1", locationDescription="You are at the ocean. You see another island in the distance...",
                        east=Just beach1}
beach1 = newLocation{locationID=20, locationName="beach1", locationDescription="You are on a sandy beach.",
                        locationGameObjects=[palm], north=Just fields1, east=Just fields2, south=Just beach2, west=Just ocean1}
fields2 = newLocation{locationID=21, locationName="fields2", locationDescription="You are in the fields. You see fallen trees, scorched earth, and a plane wreck to the north.",
                        north=Just planeWreck, east=Just crossroads, west=Just beach1}
crossroads = newLocation{locationID=22, locationName="crossroads", locationDescription="You are at a crossroads. Fortunately, someone put up a signpost here.",
                        locationGameObjects=[signpost], north=Just jungle2, east=Just path1, south=Just village1, west=Just fields2}
path1 = newLocation{locationID=23, locationName="path1", locationDescription="You are on the path. You don't know where it leads yet.",
                        north=Just darkJungle2, east=Just path3, south=Just wellSquare, west=Just crossroads}
path3 = newLocation{locationID=24, locationName="path3", locationDescription="You are on the path. You don't know where it leads yet.",
                        east=Just path2, south=Just village2, west=Just path1}
path2 = newLocation{locationID=25, locationName="path2", locationDescription="You are on the path. It starts to turn north and seems to lead to the top of the mountain.",
                        locationGameObjects=[stone], north=Just mountainPath2, west=Just path3}
ocean2 = newLocation{locationID=26, locationName="ocean2", locationDescription="You are at the ocean. You see another island in the distance...",
                        east=Just beach2}
beach2 = newLocation{locationID=27, locationName="beach2", locationDescription="You are on a sandy beach.",
                        locationGameObjects=[palm, palm, stick], north=Just beach1, south=Just beach3, east=Just ocean2}
house1 = newLocation{locationID=28, locationName="house1", locationDescription="You entered an inhabited house.",
                        locationNPCs=[native], east=Just village1, south=Just pantry}
village1 = newLocation{locationID=29, locationName="village1", locationDescription="You are in the village.",
                        locationGameObjects=[note1], north=Just crossroads, east=Just wellSquare, south=Just house2, west=Just house1}
wellSquare = newLocation{locationID=30, locationName="well_square", locationDescription="You are in the main part of the village.",
                        locationGameObjects=[well], north=Just path1, east=Just village2, south=Just oldHouse, west=Just village1}
village2 = newLocation{locationID=31, locationName="village2", locationDescription="You are in the village.",
                        locationGameObjects=[string], north=Just path3, west=Just wellSquare}
ocean3 = newLocation{locationID=32, locationName="ocean3", locationDescription="You are at the ocean. You see another island in the distance...",
                        locationGameObjects=[note5, wreck], east=Just beach3}
beach3 = newLocation{locationID=33, locationName="beach3", locationDescription="You are on a sandy beach.",
                        locationGameObjects=[palm], north=Just beach2, west=Just ocean3}
pantry = newLocation{locationID=34, locationName="pantry", locationDescription="You entered the pantry. Many shelves are empty.",
                        locationGameObjects=[meat], north=Just house1, locationDark=True}
house2 = newLocation{locationID=35, locationName="house2", locationDescription="You entered a wooden hut. Probably a weaver house.",
                        locationGameObjects=[cloth, sheets], north=Just village1}
oldHouse = newLocation{locationID=36, locationName="old_house", locationDescription="You entered the old house. It is probably a forge.", 
                        locationGameObjects=[blastFurnace], locationNPCs=[blacksmith], north=Just wellSquare, east=Just basement}
basement = newLocation{locationID=37, locationName="basement", locationDescription="You entered the dark basement.",
                        locationGameObjects=[anvil], west=Just oldHouse, locationDark=True}

-- objects
totem = newGameObject{objectName="totem", objectReusable=True}
tree = newGameObject{objectName="tree"}
stick = newGameObject{objectName="stick", objectPickable=True}
bananaTree = newGameObject{objectName="banana_tree", objectReusable=True}
lavaSource = newGameObject{objectName="lava_source", objectReusable=True}
pond = newGameObject{objectName="pond", objectReusable=True}
rose = newGameObject{objectName="rose", objectPickable=True, objectDescription="It's beautiful flower."}
dandelion = newGameObject{objectName="dandelion", objectPickable=True, objectDescription="Common flower."}
bush = newGameObject{objectName="bush"}
palm = newGameObject{objectName="palm"}
signpost = newGameObject{objectName="signpost", objectDescription="North - trees, East - hot mountaint, South - people, West - a lot of water"}
stone = newGameObject{objectName="stone", objectPickable=True}
well = newGameObject{objectName="well", objectDescription="The winch works, but the bucket is missing.", objectReusable=True}
string = newGameObject{objectName="string", objectPickable=True}
wreck = newGameObject{objectName="wreck", objectDescription="A very primitive boat made up of several logs and lianas. But it looks like it was burnt?"}
meat = newGameObject{objectName="meat", objectPickable=True}
cloth = newGameObject{objectName="cloth", objectPickable=True, objectDescription="It seems to be flammable."}
sheets = newGameObject{objectName="sheets", objectPickable=True}
blastFurnace = newGameObject{objectName="blast_furnace", objectReusable=True}
anvil = newGameObject{objectName="anvil", objectReusable=True}
banana = newGameObject{objectName="banana", objectPickable=True}
wood = newGameObject{objectName="wood", objectPickable=True}
mast = newGameObject{objectName="mast", objectPickable=True}
brushwood = newGameObject{objectName="brushwood", objectPickable=True, objectDescription="A bunch of dry branches."}
torch = newGameObject{objectName="torch", objectPickable=True}
flamingTorch = newGameObject{objectName="flaming_torch", objectPickable=True, objectDescription="The light of this torch can light up the darkness."}
rawIron = newGameObject{objectName="raw_iron", objectPickable=True}
fishingRod = newGameObject{objectName="fishing_rod", objectPickable=True, objectReusable=True}
liquidIron = newGameObject{objectName="liquid_iron", objectPickable=True}
swordForm = newGameObject{objectName="sword_form", objectPickable=True}
fish = newGameObject{objectName="fish", objectPickable=True}
hotSword = newGameObject{objectName="hot_sword", objectPickable=True}
waterBucket = newGameObject{objectName="water_bucket", objectPickable=True}
sword = newGameObject{objectName="sword", objectPickable=True, objectReusable=True}
hardwood = newGameObject{objectName="hardwood", objectPickable=True}
deck = newGameObject{objectName="deck", objectPickable=True}
sail = newGameObject{objectName="sail", objectPickable=True}
raft = newGameObject{objectName="raft", objectPickable=True}
ax = newGameObject{objectName="ax", objectPickable=True, objectDescription="Old, but perfect for cutting trees.", objectReusable=True}
rope = newGameObject{objectName="rope", objectPickable=True}
aMap = newGameObject{objectName="map", objectPickable=True, objectDescription="Thanks to the map, you can discover and remember new areas on the island."}
mysteriousStone = newGameObject{objectName="mysterious_stone", objectPickable=True}
bucket = newGameObject{objectName="bucket", objectPickable=True}
pickax = newGameObject{objectName="pickax", objectPickable=True, objectDescription="Slightly rusty, but still fit for work.", objectReusable=True}
ironOre = newGameObject{objectName="iron_ore"}
phone = newGameObject{objectName="phone", objectPickable=True, objectDescription="Works, but no signal."}
note1 = newGameObject{objectName="note1", objectPickable=True, 
                objectDescription="\"We got to the village. The people are primitive, but they understand our language very well. He told us to go to the \"hot mountain\". Perhaps we will be able to call for help from on high.\""}
note2 = newGameObject{objectName="note2", objectPickable=True, 
                objectDescription="\"We reached the top of the \"hot mountain\", which turned out to be an active volcano. We tried sending smoke signs but unfortunately it didn't help. We are now going to the cave at the foot of the volcano. Apparently a wise man lives there, and besides (unreadable)\""}
note3 = newGameObject{objectName="note3", objectPickable=True, 
                objectDescription="\"We talked to the old native in the cave. He instructed us to go to a nearby island. He is said to be more civilized than this one. We saw her from the top of the volcano. We just need to build a ship. We go west, where there is the best access to the ocean.\""}
note4 = newGameObject{objectName="note4", objectPickable=True, 
                objectDescription="\"We cut down some trees. One of them was really big. It grew next to a primitive totem in the north of the island. The natives mentioned something as a special tree, but we didn't care. We are going to the ocean now to sail to another island.\""}
note5 = newGameObject{objectName="note5", objectPickable=True, 
                objectDescription="\"We built a boat, we're le\"\nThe note ends in half a sentence."}
notebook = newGameObject{objectName="notebook", objectPickable=True, 
                objectDescription="This looks like the diary of one of the passengers. The handwriting is very blurry.\n\"We have a damaged engine, the pilot says we have to make an emergency landing on (unreadable).\"\n\"We crashed, only three of us survived. Our little group of survivors will try to find rescue. We tried to find all (unreadable), but we may have missed someone. So I leave this (unreadable). If you read this - find us. We're going (unreadable).\"\nThe next pages are torn out."}
people = newGameObject{objectName="people", objectPickable=True, 
                objectDescription="Thank God you're here! This tree that we cut down was really magical! When we tried to launch our boat, a big storm broke out. Lightning struck all around us, and finally one hit our boat and sent us to nothingness. You save our lives."}

blankGameObject = newGameObject

-- recipes
recipeBanana = newRecipe{recipeGameObject1=stone, recipeGameObject2=bananaTree, recipeProduct=banana}
recipeWood = newRecipe{recipeGameObject1=ax, recipeGameObject2=tree, recipeProduct=wood}
recipeMast = newRecipe{recipeGameObject1=ax, recipeGameObject2=palm, recipeProduct=mast}
recipeBrushwood1 = newRecipe{recipeGameObject1=ax, recipeGameObject2=bush, recipeProduct=brushwood}
recipeBrushwood2 = newRecipe{recipeGameObject1=sword, recipeGameObject2=bush, recipeProduct=brushwood}
recipeTorch1 = newRecipe{recipeGameObject1=brushwood, recipeGameObject2=cloth, recipeProduct=torch}
recipeTorch2 = newRecipe{recipeGameObject1=stick, recipeGameObject2=cloth, recipeProduct=torch}
recipeFlamingTorch1 = newRecipe{recipeGameObject1=torch, recipeGameObject2=lavaSource, recipeProduct=flamingTorch}
recipeFlamingTorch2 = newRecipe{recipeGameObject1=torch, recipeGameObject2=blastFurnace, recipeProduct=flamingTorch}
recipeRawIron = newRecipe{recipeGameObject1=pickax, recipeGameObject2=ironOre, recipeProduct=rawIron}
recipeFishingRod = newRecipe{recipeGameObject1=stick, recipeGameObject2=string, recipeProduct=fishingRod}
recipeLiquidIron = newRecipe{recipeGameObject1=blastFurnace, recipeGameObject2=rawIron, recipeProduct=liquidIron}
recipeSwordForm = newRecipe{recipeGameObject1=liquidIron, recipeGameObject2=stick, recipeProduct=swordForm}
recipeFish = newRecipe{recipeGameObject1=pond, recipeGameObject2=fishingRod, recipeProduct=fish}
recipeHotSword = newRecipe{recipeGameObject1=swordForm, recipeGameObject2=anvil, recipeProduct=hotSword}
recipeWaterBucket1 = newRecipe{recipeGameObject1=pond, recipeGameObject2=bucket, recipeProduct=waterBucket}
recipeWaterBucket2 = newRecipe{recipeGameObject1=well, recipeGameObject2=bucket, recipeProduct=waterBucket}
recipeSword1 = newRecipe{recipeGameObject1=hotSword, recipeGameObject2=waterBucket, recipeProduct=sword}
recipeSword2 = newRecipe{recipeGameObject1=pond, recipeGameObject2=hotSword, recipeProduct=sword}
recipeHardwood = newRecipe{recipeGameObject1=wood, recipeGameObject2=wood, recipeProduct=hardwood}
recipeDeck = newRecipe{recipeGameObject1=hardwood, recipeGameObject2=rope, recipeProduct=deck}
recipeSail = newRecipe{recipeGameObject1=sheets, recipeGameObject2=mast, recipeProduct=sail}
recipeRaft = newRecipe{recipeGameObject1=deck, recipeGameObject2=sail, recipeProduct=raft}
recipePeople = newRecipe{recipeGameObject1=totem, recipeGameObject2=mysteriousStone, recipeProduct=people}

-- exchanges
exchangeAx = newExchange{neededGameObject=phone, offeredGameObject=ax, 
                            exchangeDescription="Whoa! Shiny! Interesting. Take my ax."}
exchangeRope = newExchange{neededGameObject=banana, offeredGameObject=rope, 
                            exchangeDescription="This monkey was obviously just hungry. Monkey took the fruit and walked away. You can safely take the rope it was hanging on."}
exchangeMap = newExchange{neededGameObject=rose, offeredGameObject=aMap, 
                            exchangeDescription="Beautiful! Great idea! Thank you. Take this map. Thanks to it you will not get lost."}
exchangeMysteriousStone = newExchange{neededGameObject=sword, offeredGameObject=mysteriousStone, 
                            exchangeDescription="Yes! Now I feel like I can go in peace! So that's what I missed! Thank you! Please take this stone. The ghosts told me to keep an eye on him. Goodbye..."}
exchangeBanana = newExchange{neededGameObject=meat, offeredGameObject=banana,
                            exchangeDescription="Delicious! Thank you! In return, I can give you this fruit that they keep feeding me."}
exchangeBucket = newExchange{neededGameObject=dandelion, offeredGameObject=bucket, 
                            exchangeDescription="Thank you friend. An ordinary flower, and so pleasing to the eye. It's not much, but that's all I can give you."}
exchangePickax = newExchange{neededGameObject=fish, offeredGameObject=pickax, 
                            exchangeDescription="It looks delicious! Thank you! Take my pickaxe and mine some ore."}
exchangeSword = newExchange{neededGameObject=rawIron, offeredGameObject=sword, 
                            exchangeDescription="Looks like high quality ore. I'm about to forge a decent sword out of it."}

-- NPCs
native = newNPC{npcName="native", npcExchanges=[exchangeAx, exchangeMap],
                npcSpeech="Hello Stranger. You seem like a good man. I have a request for you. My father lives in a cave in the north of the island. I'd like to take him some meat, but I haven't had time for that lately. Could you do it for me? My father will be grateful. You can get the meat from the cellar. By the way... I would like to give my chosen one a little thing, but I have no idea. Could you please find something for me? I'm afraid to walk in the jungle. If you help me, I'll give you my old map. Maybe it will be useful to you. I can also exchange my ax for some interesting object."}
oldNative = newNPC{npcName="old_native", npcExchanges=[exchangeBucket, exchangeBanana],
                npcSpeech="Hello friend. I am m'Ilio. I used to be a village chief, but I stepped back into the shadows after losing the battle with the invaders. I would like to leave this cave, but I am afraid of the reaction of the other inhabitants. However, I will be very grateful to you if you bring me something that will make me remember about the outside world even for a short time. I am also very hungry. My son brings me food sometimes, but it's usually fruit or nuts. I want meat. Bring them to me and I will reward you."}
monkey = newNPC{npcName="monkey", npcExchanges=[exchangeRope], 
                npcDescription="It's definitely a bad monkey. Why? Interestingly, instead of on the liana, the monkey hangs on a fairly solid rope.",
                npcSpeech="U-u-aaaa! Buaaaaa!"}
ancientGuard = newNPC{npcName="ancient_guard", npcExchanges=[exchangeMysteriousStone],
                npcDescription="The statue shows a knight in full armor. However, his hands are empty...",
                npcSpeech="Hello traveler. I used to be a ruthless knight but was cursed by the village shaman. Now I am only a stone statue. The shaman said the curse would be lifted if I became a \"real warrior\". I do not know what it means. Help me please and I will reward you generously."}
blacksmith = newNPC{npcName="blacksmith", npcExchanges=[exchangePickax, exchangeSword],
                npcSpeech="Hello traveler. I am a local blacksmith. I could forge a simple sword for you or let you use my workshop, but unfortunately I don't have the right resources. I heard that there are still deposits of iron left in the cave in the north of the island. If you give me a fish, I will give you my pickaxe so that you can mine the ore."}

-- describing
join :: [GameObject] -> String
join [] = ""
join [object] = objectName object
join (object:objects) = (objectName object) ++ ", " ++ (join objects)

joinNPC :: [NPC] -> String
joinNPC [] = ""
joinNPC [npc] = npcName npc
joinNPC (npc:npcs) = (npcName npc) ++ ", " ++ (joinNPC npcs)

printMessage :: String -> GameState -> GameState
printMessage msg state = state{output=msg}

describeSituation :: GameState -> IO ()
describeSituation state = printLines [output state, ""]

look :: GameState -> GameState
look state = printMessage ((locationDescription location)++"\n\n"++desc) state
    where
        location = currentLocation state
        desc = 
            if isVisible location (playerGameObjects state) then
                description (locationGameObjects (currentLocation state)) (locationNPCs (currentLocation state))
            else 
                "You see darkness only"   

isVisible :: Location -> [GameObject] -> Bool
isVisible location inventory = not (locationDark location && not (elem flamingTorch inventory))

description :: [GameObject] -> [NPC] -> String
description objects npcs = 
    if (length objects) > 0 && (length npcs) > 0 then
        "There are: " ++ (join objects) ++ ", " ++ (joinNPC npcs)
    else if (length objects) > 0 then
        "There are: " ++ (join objects)
    else if (length npcs) > 0 then
        "There are: " ++ (joinNPC npcs)
    else
        "There is nothing."

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
moveTo location state = look state{currentLocation=newLocation, output="",
                            discoveredLocationsID=newIDs, visitedLocations=newLocations}
    where
        id = locationID location
        locationsID = discoveredLocationsID state
        locations = visitedLocations state

        oldLocation = currentLocation state
        oldID = locationID oldLocation
        newLocation = 
            case (findLocationByID id locations) of
                Nothing -> location
                Just loc -> loc
        newLocations = oldLocation:(removeLocationByID (locationID oldLocation) locations)
        newIDs = 
            if elem aMap (playerGameObjects state) && not (elem id locationsID) then
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

findLocationByID :: Integer -> [Location] -> Maybe Location
findLocationByID id [] = Nothing
findLocationByID id (location:locations) =
    if locationID location == id then
        Just location
    else
        findLocationByID id locations

removeLocationByID :: Integer -> [Location] -> [Location]
removeLocationByID _ [] = []
removeLocationByID id (location:locations) | id == (locationID location) = removeLocationByID id locations
                                            | otherwise = location : removeLocationByID id locations

getNameLocation :: Location -> GameState -> String
getNameLocation location state | not (elem id (discoveredLocationsID state)) = "undiscovered"
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

-- interacting with NPCs
exchangeInList :: GameObject -> [Exchange] -> Maybe Exchange
exchangeInList object [] = Nothing
exchangeInList object (exchange:exchanges) = do
    if object == neededGameObject exchange then
        Just exchange
    else
        exchangeInList object exchanges

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

examineNPC :: NPC -> GameState -> GameState
examineNPC npc state = printMessage (message npc) state
    where
        message :: NPC -> String
        message npc = 
            if (npcDescription npc) /= "" then
                npcDescription npc
            else
                ("You don't know anything about "++(npcName npc))

give :: String -> String -> GameState -> GameState
give objName npName state = prepareExchange object npc state
    where
        npc = npcInList npName (locationNPCs (currentLocation state))
        object = objectInList objName (playerGameObjects state)

        prepareExchange :: Maybe GameObject -> Maybe NPC -> GameState -> GameState
        prepareExchange object npc state = 
            case object of
                Nothing -> printMessage ("You don't have "++objName) state
                Just object -> 
                    case npc of
                        Nothing -> printMessage ("There is no "++npName) state
                        Just npc -> tryExchange (exchangeInList object (npcExchanges npc)) state

tryExchange :: Maybe Exchange -> GameState -> GameState
tryExchange exchange state =
    case exchange of
        Nothing -> printMessage "He doesn't want it" state
        Just exchange -> doExchange exchange state

doExchange :: Exchange -> GameState -> GameState
doExchange exchange state = state{output=desc++"\nYou got "++(objectName gift), 
                                    playerGameObjects=gift:inventory}
    where
        desc = exchangeDescription exchange
        gift = offeredGameObject exchange
        inventory = removeGameObject (neededGameObject exchange) (playerGameObjects state) 

-- interact with objects
showInventory :: GameState -> GameState
showInventory state = printMessage (descGameObjects) state
    where
        descGameObjects = join (playerGameObjects state)

doExamination :: String -> GameState -> GameState
doExamination name state = 
    case npc of
        Just npc -> examineNPC npc state
        Nothing -> case object of
            Just object -> examine object state
            Nothing -> printMessage ("There is no "++name) state

    where
        npc = npcInList name (locationNPCs (currentLocation state))
        object = objectInList name ((locationGameObjects (currentLocation state))++(playerGameObjects state))

examine :: GameObject -> GameState -> GameState
examine object state = printMessage (message object) state
    where 
        message :: GameObject -> String
        message it = 
            if (objectDescription it) /= "" then
                objectDescription it
            else
                "You don't know anything about "++(objectName it)

findRecipe :: GameObject -> GameObject -> [Recipe] -> Maybe Recipe
findRecipe _ _ [] = Nothing
findRecipe object1 object2 (recipe:recipes) = 
    if (recipeGameObject1 recipe) == object1 && (recipeGameObject2 recipe) == object2 || 
       (recipeGameObject2 recipe) == object1 && (recipeGameObject1 recipe) == object2 then
        Just recipe
    else
        findRecipe object1 object2 recipes 

useUp :: GameObject -> GameState -> GameState
useUp object state = 
    if elem object (playerGameObjects state) then
        if objectReusable object then
            state
        else
            state{playerGameObjects=inventory}
    else
        if objectReusable object then
            state
        else
            state{currentLocation=location{locationGameObjects=locGameObjects}}
    
        where
            inventory = removeGameObject object (playerGameObjects state)
            location = currentLocation state
            locGameObjects = removeGameObject object (locationGameObjects location)

craftGameObject :: GameObject -> GameObject -> Recipe -> GameState -> GameState
craftGameObject object1 object2 recipe state = newState{playerGameObjects=object:inventory, output="You got "++(objectName object)}
    where
        object = recipeProduct recipe
        newState = useUp object2 (useUp object1 state)
        inventory = playerGameObjects newState

checkRecipe :: GameObject -> GameObject -> GameState -> GameState
checkRecipe object1 object2 state = 
    case recipe of
        Nothing -> printMessage "You can't use it that way" state
        Just recipe -> craftGameObject object1 object2 recipe state
    where
        recipe = findRecipe object1 object2 (recipes state)

isAvailable :: String -> GameState -> GameObject
isAvailable objName state = 
    case object of
        Nothing -> blankGameObject
        Just object -> object

        where
            object = objectInList objName ((locationGameObjects (currentLocation state))++(playerGameObjects state))

use :: String -> String -> GameState -> GameState
use objectName1 objectName2 state = tryCraft object1 object2 state
    where
        object1 = isAvailable objectName1 state
        object2 = isAvailable objectName2 state

        tryCraft ::GameObject -> GameObject -> GameState -> GameState
        tryCraft object1 object2 = do
            if object1 == blankGameObject then
                printMessage ("You don't have "++objectName1)
            else
                if object2 == blankGameObject then
                    printMessage ("You don't have "++objectName2)
                else
                    checkRecipe object1 object2

objectInList :: String -> [GameObject] -> Maybe GameObject
objectInList objName [] = Nothing
objectInList objName (object:objects) = do
    if objName == name then
        Just object
    else 
        objectInList objName objects
    where
        name = (objectName object)

pickUp :: String -> GameState -> GameState
pickUp objectName state = tryPut object state
    where
    object = objectInList objectName (locationGameObjects (currentLocation state))
    tryPut it =
        case it of
            Nothing -> printMessage ("There is no "++objectName)
            Just it -> if objectPickable it then 
                            putInventory it 
                       else 
                            printMessage "You can't do this"

dropGameObject :: String -> GameState -> GameState
dropGameObject objName state = tryDrop object state
    where
        object = objectInList objName (playerGameObjects state)

        tryDrop :: Maybe GameObject -> GameState -> GameState
        tryDrop object state = 
            case object of
                Nothing -> printMessage ("You don't have "++objName) state
                Just object -> state{output="You dropped "++objName, 
                                    playerGameObjects=removeGameObject object (playerGameObjects state),
                                    currentLocation=(currentLocation state){locationGameObjects=object:(locationGameObjects (currentLocation state))}
                                    }

removeGameObject :: GameObject -> [GameObject] -> [GameObject]
removeGameObject object list = List.delete object list

putInventory :: GameObject -> GameState -> GameState
putInventory object state = state{currentLocation=location{locationGameObjects=objects}, 
                                playerGameObjects=object:inventory, 
                                output="You picked up " ++ (objectName object)}
    where 
        inventory = playerGameObjects state
        location = currentLocation state
        objects = removeGameObject object (locationGameObjects location)

-- leaving an island
atOcean :: GameState -> Bool
atOcean state = elem (locationID (currentLocation state)) [19, 26, 32]

hasNotes :: GameState -> Bool
hasNotes state =
    elem note1 (playerGameObjects state) && elem note2 (playerGameObjects state) && elem note3 (playerGameObjects state) &&
    elem note4 (playerGameObjects state) && elem note5 (playerGameObjects state)

hasRaft :: GameState -> Bool
hasRaft state = elem raft (playerGameObjects state)

hasPeople :: GameState -> Bool
hasPeople state = elem people (playerGameObjects state)

leave :: GameState -> GameState            
leave state = 
    if hasRaft state && atOcean state then
        if hasNotes state && hasPeople state then
            state{gameOver=True, output="You left the island. You rescued rest of the passengers and complete the story. You won!"}
        else if hasNotes state then
            state{gameOver=True, output="You left the island. You complete the story, but didn't find rest of the passengers."}
        else if hasPeople state then
            state{gameOver=True, output="You left the island. You found the rest of the passengers, but didn't know their full story."}
        else
            state{gameOver=True, output="You left the island, but dont't know anything."}   
    else if hasRaft state then
        printMessage "You aren't at the ocean" state
    else 
        printMessage "You can't swim across the ocean. You need boat." state

-- commands
readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs

showInstructions :: GameState -> GameState
showInstructions state = state{output=instructions}
    where
        instructions = List.intercalate "\n" instructionsText

quit :: GameState -> GameState
quit state = state{gameOver=True, output="Game quitted"}

tryCommandZero :: (GameState -> GameState) -> String -> (GameState -> GameState)
tryCommandZero command input = 
    if length (words input) == 1 then
        command
    else
        \state -> state{output="Wrong number of arguments. Required: 1"}

tryCommandOne :: (String -> GameState -> GameState) -> String -> (GameState -> GameState)
tryCommandOne command input = 
    if length (words input) == 1 then
        command input
    else
        \state -> state{output="Wrong number of arguments. Required: 1"}

tryCommandTwo :: (String -> GameState -> GameState) -> String -> (GameState -> GameState)
tryCommandTwo command input = 
    if length (words input) == 2 then
        command ((words input)!!1)
    else
        \state -> state{output="Wrong number of arguments. Required: 2"}

tryCommandThree :: (String -> String -> GameState -> GameState) -> String -> (GameState -> GameState)
tryCommandThree command input = 
    if length (words input) == 3 then
        command ((words input)!!1) ((words input)!!2)
    else
        \state -> state{output="Wrong number of arguments. Required: 3"}

parseCommand :: String -> GameState -> GameState
parseCommand input = 
    if input /= "" then
        case head (words input) of
            "n" -> tryCommandOne move input
            "s" -> tryCommandOne move input
            "w" -> tryCommandOne move input
            "e" -> tryCommandOne move input
            "examine" -> tryCommandTwo doExamination input
            "use" -> tryCommandThree use input
            "inventory" -> tryCommandZero showInventory input
            "take" -> tryCommandTwo pickUp input
            "drop" -> tryCommandTwo dropGameObject input
            "talk" -> tryCommandTwo talk input
            "give" -> tryCommandThree give input
            "look" -> tryCommandZero look input
            "look_around" -> tryCommandZero lookAround input
            "leave" -> tryCommandZero leave input
            "instructions" -> tryCommandZero showInstructions input
            "halt" -> tryCommandZero quit input
            otherwise -> (\state -> state{output="Unrecognized command"})
    else
        \state -> state{output="Unrecognized command"}

startingState :: IO GameState
startingState = return (look (GameState fields2 False "" [phone] recipes [] []))
    where
        recipes = [recipeBanana, recipeWood, recipeMast, recipeBrushwood1, recipeBrushwood2, recipeTorch1,
                    recipeTorch2, recipeFlamingTorch1, recipeFlamingTorch2, recipeRawIron, recipeFishingRod,
                    recipeLiquidIron, recipeSwordForm, recipeFish, recipeHotSword, recipeWaterBucket1,
                    recipeWaterBucket2, recipeSword1, recipeSword2, recipeHardwood, recipeDeck, recipeSail,
                    recipeRaft, recipePeople]

gameLoop :: IO ()
gameLoop = do
    state <- startingState
    finalState <- play state
    putStrLn (output finalState)
    return ()
    where
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
