import Data.List as List

data GameState = GameState 
    { currentLocation       :: Location
    , gameOver              :: Bool
    , output                :: String
    , playerItems           :: [Item]
    , recipes               :: [Recipe]
    , locationsDiscovered   :: [Integer]
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
    ,   locationItems       :: [Item]
    ,   locationNPCs        :: [NPC]
    ,   locationDark        :: Bool
    } deriving (Eq, Show)

data Item = Item
    {
        itemName            :: String
    ,   itemDescription     :: String
    ,   itemPickable        :: Bool
    ,   itemReusable        :: Bool
    } deriving (Eq, Show)

data Recipe = Recipe
    {
        recipeItem1         :: Item
    ,   recipeItem2         :: Item
    ,   recipeProduct       :: Item
    } deriving (Eq, Show)

data Exchange = Exchange
    {
        neededItem          :: Item
    ,   offeredItem         :: Item
    ,   exchangeDescription :: String
    } deriving (Eq, Show)

data NPC = NPC
    {
        npcName             :: String
    ,   npcSpeech           :: String
    ,   npcDescription      :: String
    ,   npcExchanges        :: [Exchange]
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
    itemPickable = False,
    itemReusable = False
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
                        locationItems=[totem], east=Just forest1, south=Just forest2}
forest1 = newLocation{locationID=2, locationName="forest1", locationDescription="You are in the forest.",
                        locationItems=[tree, tree, stick], east=Just darkJungle1, south=Just meadow2, west=Just meadow1}
darkJungle1 = newLocation{locationID=3, locationName="dark_jungle1", locationDescription="You entered the very dense jungle.", 
                        locationItems=[bananaTree], east=Just tunnelEntrance, south=Just jungle1, west=Just forest1, locationDark=True}
tunnelEntrance = newLocation{locationID=4, locationName="tunnel_entrance", locationDescription="You entered a dark tunnel.",
                        south=Just tunnel1, west=Just darkJungle1, locationDark=True}
ancientRuin = newLocation{locationID=5, locationName="ancient_ruin", locationDescription="You are in a stone crypt. You can see strange signs on the walls, and in the middle - a rock statue of a knight.",
                        locationNPCs=[ancientGuard], south=Just tunnel2, locationDark=True}
volcanoPeak = newLocation{locationID=6, locationName="volcano_peak", locationDescription="You reached the top. But it is not the top of the mountain. This is the top of the volcano!",
                        locationItems=[lavaSource], south=Just mountainPath1}
forest2 = newLocation{locationID=7, locationName="forest2", locationDescription="You are in the forest.",
                        locationItems=[tree, tree], north=Just meadow1, east=Just meadow2, south=Just fields1}
meadow2 = newLocation{locationID=8, locationName="meadow2", locationDescription="You see a pond in the middle of a flower meadow.", 
                        locationItems=[pond, rose, dandelion], north=Just forest1, east=Just jungle1, west=Just forest2}
jungle1 = newLocation{locationID=9, locationName="jungle1", locationDescription="You are in the jungle.", 
                        locationItems=[bush, tree], locationNPCs=[monkey],
                        north=Just darkJungle1, south=Just jungle2, west=Just meadow2}
tunnel1 = newLocation{locationID=10, locationName="tunnel1", locationDescription="You are in a long rock corridor. You don't see much.",
                        locationItems=[note3], north=Just tunnelEntrance, east=Just tunnel2, locationDark=True}
tunnel2 = newLocation{locationID=11, locationName="tunnel2", locationDescription="You reached a fork. The walls of the tunnel have a strange color...",
                        locationItems=[ironOre], north=Just ancientRuin, south=Just cave, west=Just tunnel1, locationDark=True}
mountainPath1 = newLocation{locationID=12, locationName="mountain_path1", locationDescription="You are getting closer to the top of the mountain. It''s getting hot...",
                        locationItems=[note2], north=Just volcanoPeak, south=Just mountainPath2}
fields1 = newLocation{locationID=13, locationName="fields1", locationDescription="You are in the fields.", 
                        locationItems=[note4], north=Just forest1, south=Just beach1}
planeWreck = newLocation{locationID=14, locationName="plane_wreck", locationDescription="You entered the plane wreck. Certainly, more than one person flew on it, unfortunately you do not see anyone.",
                        locationItems=[notebook], south=Just fields2}
jungle2 = newLocation{locationID=15, locationName="jungle2", locationDescription="You are in the jungle.", locationItems=[bush],
                        north=Just jungle1, east=Just darkJungle2, south=Just crossroads}
darkJungle2 = newLocation{locationID=16, locationName="dark_jungle2", locationDescription="You entered the very dense jungle.",
                        locationItems=[bush, stick], south=Just path1, west=Just jungle2, locationDark=True}
cave = newLocation{locationID=17, locationName="cave", locationDescription="You have entered a dark cave. You are clearly not alone here...",
                        locationNPCs=[oldNative], north=Just tunnel2, locationDark=True}
mountainPath2 = newLocation{locationID=18, locationName="mountain_path2", locationDescription="You start climbing the mountain path.",
                        north=Just mountainPath1, south=Just path2}
ocean1 = newLocation{locationID=19, locationName="ocean1", locationDescription="You are at the ocean. You see another island in the distance...",
                        east=Just beach1}
beach1 = newLocation{locationID=20, locationName="beach1", locationDescription="You are on a sandy beach.",
                        locationItems=[palm], north=Just fields1, east=Just fields2, south=Just beach2, west=Just ocean1}
fields2 = newLocation{locationID=21, locationName="fields2", locationDescription="You are in the fields. You see fallen trees, scorched earth, and a plane wreck to the north.",
                        north=Just planeWreck, east=Just crossroads, west=Just beach1}
crossroads = newLocation{locationID=22, locationName="crossroads", locationDescription="You are at a crossroads. Fortunately, someone put up a signpost here.",
                        locationItems=[signpost], north=Just jungle2, east=Just path1, south=Just village1, west=Just fields2}
path1 = newLocation{locationID=23, locationName="path1", locationDescription="You are on the path. You don''t know where it leads yet.",
                        north=Just darkJungle2, east=Just path3, south=Just wellSquare, west=Just crossroads}
path3 = newLocation{locationID=24, locationName="path3", locationDescription="You are on the path. You don''t know where it leads yet.",
                        east=Just path2, south=Just village2, west=Just path1}
path2 = newLocation{locationID=25, locationName="path2", locationDescription="You are on the path. It starts to turn north and seems to lead to the top of the mountain.",
                        locationItems=[stone], north=Just mountainPath2, west=Just path3}
ocean2 = newLocation{locationID=26, locationName="ocean2", locationDescription="You are at the ocean. You see another island in the distance...",
                        east=Just beach2}
beach2 = newLocation{locationID=27, locationName="beach2", locationDescription="You are on a sandy beach.",
                        locationItems=[palm, palm, stick], north=Just beach1, south=Just beach3, east=Just ocean2}
house1 = newLocation{locationID=28, locationName="house1", locationDescription="You entered an inhabited house.",
                        locationNPCs=[native], east=Just village1, south=Just pantry}
village1 = newLocation{locationID=29, locationName="village1", locationDescription="You are in the village.",
                        locationItems=[note1], north=Just crossroads, east=Just wellSquare, south=Just house2, west=Just house1}
wellSquare = newLocation{locationID=30, locationName="well_square", locationDescription="You are in the main part of the village.",
                        locationItems=[well], north=Just path1, east=Just village2, south=Just oldHouse, west=Just village1}
village2 = newLocation{locationID=31, locationName="village2", locationDescription="You are in the village.",
                        locationItems=[string], north=Just path3, west=Just wellSquare}
ocean3 = newLocation{locationID=32, locationName="ocean3", locationDescription="You are at the ocean. You see another island in the distance...",
                        locationItems=[note5, wreck], east=Just beach3}
beach3 = newLocation{locationID=33, locationName="beach3", locationDescription="You are on a sandy beach.",
                        locationItems=[palm], north=Just beach2, west=Just ocean3}
pantry = newLocation{locationID=34, locationName="pantry", locationDescription="You entered the pantry. Many shelves are empty.",
                        locationItems=[meat], north=Just house1, locationDark=True}
house2 = newLocation{locationID=35, locationName="house2", locationDescription="You entered a wooden hut. Probably a weaver house.",
                        locationItems=[cloth, sheets], north=Just village1}
oldHouse = newLocation{locationID=36, locationName="old_house", locationDescription="You entered the old house. It is probably a forge.", 
                        locationItems=[blastFurnace], locationNPCs=[blacksmith], north=Just wellSquare, east=Just basement}
basement = newLocation{locationID=37, locationName="basement", locationDescription="You entered the dark basement.",
                        locationItems=[anvil], west=Just oldHouse, locationDark=True}


-- items
totem = newItem{itemName="totem", itemReusable=True}
tree = newItem{itemName="tree"}
stick = newItem{itemName="stick", itemPickable=True}
bananaTree = newItem{itemName="banana_tree", itemReusable=True}
lavaSource = newItem{itemName="lava_source", itemReusable=True}
pond = newItem{itemName="pond", itemReusable=True}
rose = newItem{itemName="rose", itemPickable=True, itemDescription="It's beautiful flower."}
dandelion = newItem{itemName="dandelion", itemPickable=True, itemDescription="Common flower."}
bush = newItem{itemName="bush"}
palm = newItem{itemName="palm"}
signpost = newItem{itemName="signpost", itemDescription="North - trees, East - hot mountaint, South - people, West - a lot of water"}
stone = newItem{itemName="stone", itemPickable=True}
well = newItem{itemName="well", itemDescription="The winch works, but the bucket is missing.", itemReusable=True}
string = newItem{itemName="string", itemPickable=True}
wreck = newItem{itemName="wreck", itemDescription="A very primitive boat made up of several logs and lianas. But it looks like it was burnt?"}
meat = newItem{itemName="meat", itemPickable=True}
cloth = newItem{itemName="cloth", itemPickable=True, itemDescription="It seems to be flammable."}
sheets = newItem{itemName="sheets", itemPickable=True}
blastFurnace = newItem{itemName="blast_furnace", itemReusable=True}
anvil = newItem{itemName="anvil", itemReusable=True}
banana = newItem{itemName="banana", itemPickable=True}
wood = newItem{itemName="wood", itemPickable=True}
mast = newItem{itemName="mast", itemPickable=True}
brushwood = newItem{itemName="brushwood", itemPickable=True, itemDescription="A bunch of dry branches."}
torch = newItem{itemName="torch", itemPickable=True}
flamingTorch = newItem{itemName="flaming_torch", itemPickable=True, itemDescription="The light of this torch can light up the darkness."}
rawIron = newItem{itemName="raw_iron", itemPickable=True}
fishingRod = newItem{itemName="fishing_rod", itemPickable=True, itemReusable=True}
liquidIron = newItem{itemName="liquid_iron", itemPickable=True}
swordForm = newItem{itemName="sword_form", itemPickable=True}
fish = newItem{itemName="fish", itemPickable=True}
hotSword = newItem{itemName="hot_sword", itemPickable=True}
waterBucket = newItem{itemName="water_bucket", itemPickable=True}
sword = newItem{itemName="sword", itemPickable=True, itemReusable=True}
hardwood = newItem{itemName="hardwood", itemPickable=True}
deck = newItem{itemName="deck", itemPickable=True}
sail = newItem{itemName="sail", itemPickable=True}
raft = newItem{itemName="raft", itemPickable=True}
ax = newItem{itemName="ax", itemPickable=True, itemDescription="Old, but perfect for cutting trees.", itemReusable=True}
rope = newItem{itemName="rope", itemPickable=True}
aMap = newItem{itemName="map", itemPickable=True, itemDescription="Thanks to the map, you can discover and remember new areas on the island."}
mysteriousStone = newItem{itemName="mysterious_stone", itemPickable=True}
bucket = newItem{itemName="bucket", itemPickable=True}
pickax = newItem{itemName="pickax", itemPickable=True, itemDescription="Slightly rusty, but still fit for work.", itemReusable=True}
ironOre = newItem{itemName="iron_ore"}
phone = newItem{itemName="phone", itemPickable=True, itemDescription="Works, but no signal."}
note1 = newItem{itemName="note1", itemPickable=True, 
                itemDescription="\"We got to the village. The people are primitive, but they understand our language very well. He told us to go to the \"hot mountain\". Perhaps we will be able to call for help from on high.\""}
note2 = newItem{itemName="note2", itemPickable=True, 
                itemDescription="\"We reached the top of the \"hot mountain\", which turned out to be an active volcano. We tried sending smoke signs but unfortunately it didn't help. We are now going to the cave at the foot of the volcano. Apparently a wise man lives there, and besides (unreadable)\""}
note3 = newItem{itemName="note3", itemPickable=True, 
                itemDescription="\"We talked to the old native in the cave. He instructed us to go to a nearby island. He is said to be more civilized than this one. We saw her from the top of the volcano. We just need to build a ship. We go west, where there is the best access to the ocean.\""}
note4 = newItem{itemName="note4", itemPickable=True, 
                itemDescription="\"We cut down some trees. One of them was really big. It grew next to a primitive totem in the north of the island. The natives mentioned something as a special tree, but we didn't care. We are going to the ocean now to sail to another island.\""}
note5 = newItem{itemName="note5", itemPickable=True, 
                itemDescription="\"We built a boat, we''re le\"\nThe note ends in half a sentence."}
notebook = newItem{itemName="notebook", itemPickable=True, 
                itemDescription="This looks like the diary of one of the passengers. The handwriting is very blurry.\n\"We have a damaged engine, the pilot says we have to make an emergency landing on (unreadable).\"\n\"We crashed, only three of us survived. Our little group of survivors will try to find rescue. We tried to find all (unreadable), but we may have missed someone. So I leave this (unreadable). If you read this - find us. We''re going (unreadable).\"\nThe next pages are torn out."}
people = newItem{itemName="people", itemPickable=True, 
                itemDescription="Thank God you're here! This tree that we cut down was really magical! When we tried to launch our boat, a big storm broke out. Lightning struck all around us, and finally one hit our boat and sent us to nothingness. You save our lives."}

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
recipeFish = newRecipe{recipeItem1=pond, recipeItem2=fishingRod, recipeProduct=fish}
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
exchangeAx = newExchange{neededItem=phone, offeredItem=ax, 
                            exchangeDescription="Whoa! Shiny! Interesting. Take my ax."}
exchangeRope = newExchange{neededItem=banana, offeredItem=rope, 
                            exchangeDescription="This monkey was obviously just hungry. Monkey took the fruit and walked away. You can safely take the rope it was hanging on."}
exchangeMap = newExchange{neededItem=rose, offeredItem=aMap, 
                            exchangeDescription="Beautiful! Great idea! Thank you. Take this map. Thanks to it you will not get lost."}
exchangeMysteriousStone = newExchange{neededItem=sword, offeredItem=mysteriousStone, 
                            exchangeDescription="Yes! Now I feel like I can go in peace! So that's what I missed! Thank you! Please take this stone. The ghosts told me to keep an eye on him. Goodbye..."}
exchangeBanana = newExchange{neededItem=meat, offeredItem=banana,
                            exchangeDescription="Delicious! Thank you! In return, I can give you this fruit that they keep feeding me."}
exchangeBucket = newExchange{neededItem=dandelion, offeredItem=bucket, 
                            exchangeDescription="Thank you friend. An ordinary flower, and so pleasing to the eye. It's not much, but that's all I can give you."}
exchangePickax = newExchange{neededItem=fish, offeredItem=pickax, 
                            exchangeDescription="It looks delicious! Thank you! Take my pickaxe and mine some ore."}
exchangeSword = newExchange{neededItem=rawIron, offeredItem=sword, 
                            exchangeDescription="Looks like high quality ore. I'm about to forge a decent sword out of it."}

-- NPCs
native = newNPC{npcName="native", npcExchanges=[exchangeAx, exchangeMap],
                npcSpeech="Hello Stranger. You seem like a good man. I have a request for you. My father lives in a cave in the north of the island. I'd like to take him some meat, but I haven't had time for that lately. Could you do it for me? My father will be grateful. You can get the meat from the cellar. By the way ... I would like to give my chosen one a little thing, but I have no idea. Could you please find something for me? I'm afraid to walk in the jungle. If you help me, I'll give you my old map. Maybe it will be useful to you. I can also exchange my ax for some interesting item."}
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
isVisible location inventory = not (locationDark location && not (elem flamingTorch inventory))

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
        inventory = removeItem (neededItem exchange) (playerItems state) 

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
doExamination :: String -> GameState -> GameState
doExamination name state = 
    case npc of
        Just npc -> examineNPC npc state
        Nothing -> case item of
            Just item -> examine item state
            Nothing -> printMessage ("There is no "++name) state

    where
        npc = npcInList name (locationNPCs (currentLocation state))
        item = itemInList name ((locationItems (currentLocation state))++(playerItems state))


examine :: Item -> GameState -> GameState
examine item state = printMessage (message item) state
    where 
        message :: Item -> String
        message it = 
            if (itemDescription it) /= "" then
                itemDescription it
            else
                "You don't know anything about "++(itemName it)

findRecipe :: Item -> Item -> [Recipe] -> Maybe Recipe
findRecipe _ _ [] = Nothing
findRecipe item1 item2 (recipe:recipes) = 
    if (recipeItem1 recipe) == item1 && (recipeItem2 recipe) == item2 || 
       (recipeItem2 recipe) == item1 && (recipeItem1 recipe) == item2 then
        Just recipe
    else
        findRecipe item1 item2 recipes 

useUp :: Item -> GameState -> GameState
useUp item state = 
    if elem item (playerItems state) then
        if itemReusable item then
            state
        else
            state{playerItems=inventory}
    else
        if itemReusable item then
            state
        else
            state{currentLocation=location{locationItems=locItems}}
    
        where
            inventory = removeItem item (playerItems state)
            location = currentLocation state
            locItems = removeItem item (locationItems location)

craftItem :: Item -> Item -> Recipe -> GameState -> GameState
craftItem item1 item2 recipe state = newState{playerItems=item:inventory, output="You got "++(itemName item)}
    where
        item = recipeProduct recipe
        newState = useUp item2 (useUp item1 state)
        inventory = playerItems newState

checkRecipe :: Item -> Item -> GameState -> GameState
checkRecipe item1 item2 state = 
    case recipe of
        Nothing -> printMessage "You can't use it that way" state
        Just recipe -> craftItem item1 item2 recipe state
    where
        recipe = findRecipe item1 item2 (recipes state)

isAvailable :: String -> GameState -> Item
isAvailable itName state = 
    case item of
        Nothing -> blankItem
        Just item -> item

        where
            item = itemInList itName ((locationItems (currentLocation state))++(playerItems state))

use :: String -> String -> GameState -> GameState
use itemName1 itemName2 state = tryCraft item1 item2 state
    where
        item1 = isAvailable itemName1 state
        item2 = isAvailable itemName2 state

        tryCraft ::Item -> Item -> GameState -> GameState
        tryCraft item1 item2 = do
            if item1 == blankItem then
                printMessage ("You don't have "++itemName1)
            else
                if item2 == blankItem then
                    printMessage ("You don't have "++itemName2)
                else
                    checkRecipe item1 item2

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

showInstructions :: GameState -> GameState
showInstructions state = state{output=instructions}
    where
        instructions = List.intercalate "\n" instructionsText

quit :: GameState -> GameState
quit state = state{gameOver=True, output="Game quitted"}

atOcean :: GameState -> Bool
atOcean state = elem (locationID (currentLocation state)) [19, 26, 32]


hasNotes :: GameState -> Bool
hasNotes state =
    elem note1 (playerItems state) && elem note2 (playerItems state) && elem note3 (playerItems state) &&
    elem note4 (playerItems state) && elem note5 (playerItems state)


hasRaft :: GameState -> Bool
hasRaft state = elem raft (playerItems state)


hasPeople :: GameState -> Bool
hasPeople state = elem people (playerItems state)


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

tryCommandZero :: (GameState -> GameState) -> String -> (GameState -> GameState)
tryCommandZero command input = 
    if length (words input) == 1 then
        command
    else
        \state -> state{output="Wrong number of arguments"}

tryCommandOne :: (String -> GameState -> GameState) -> String -> (GameState -> GameState)
tryCommandOne command input = 
    if length (words input) == 1 then
        command input
    else
        \state -> state{output="Wrong number of arguments"}

tryCommandTwo :: (String -> GameState -> GameState) -> String -> (GameState -> GameState)
tryCommandTwo command input = 
    if length (words input) == 2 then
        command ((words input)!!1)
    else
        \state -> state{output="Wrong number of arguments"}

tryCommandThree :: (String -> String -> GameState -> GameState) -> String -> (GameState -> GameState)
tryCommandThree command input = 
    if length (words input) == 3 then
        command ((words input)!!1) ((words input)!!2)
    else
        \state -> state{output="Wrong number of arguments"}

parseCommand :: String -> GameState -> GameState
parseCommand input = 
    case head (words input) of
        "n" -> tryCommandOne move input
        "s" -> tryCommandOne move input
        "w" -> tryCommandOne move input
        "e" -> tryCommandOne move input
        "examine" -> tryCommandTwo doExamination input
        "use" -> tryCommandThree use input
        "inventory" -> tryCommandZero showInventory input
        "take" -> tryCommandTwo pickUp input
        "drop" -> tryCommandTwo dropItem input
        "talk" -> tryCommandTwo talk input
        "give" -> tryCommandThree give input
        "look_around" -> tryCommandZero lookAround input
        "leave" -> tryCommandZero leave input
        "instructions" -> tryCommandZero showInstructions input
        "halt" -> tryCommandZero quit input
        otherwise -> (\state -> state{output="Unrecognized command"})

startingState :: IO GameState
startingState = return (GameState fields2 False "" [phone] recipes [0])
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
