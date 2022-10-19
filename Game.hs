import Base

opposite :: Direction -> Direction 
opposite direction = case direction of
    North -> South 
    South -> North 
    West  -> East 
    East  -> West 

noActions :: Item -> GameState -> Next GameState
noActions item _ = Same ("There is nothing that can be done with the " ++ show item)


winningRoom :: Room
winningRoom = Room {
    name        = "Emerald Chamber", 
    description = "your destination point. You feel marvellous as the gold, diamonds, and ancient scrolls begin to rain down on you", 
    isWinRoom   = True, 
    requires    = Just Key, 
    items       = [], 
    monsters    = [], 
    doors       = [], 
    actions     = noActions
}
    
startingRoom :: Room
startingRoom = Room {
    name        = "Dwarven Forge", 
    description = "quiet and cosy", 
    isWinRoom   = False, 
    requires    = Nothing, 
    items       = [(Spoon, "On the ancient anvil")], 
    monsters    = [], 
    doors       = [(North, winningRoom), (East, monsterRoom), (West, monsterRoom2)], 
    actions     = noActions
}

monsterRoom :: Room
monsterRoom = Room {
    name        = "Murky Garden", 
    description = "windy and blood-curdling", 
    isWinRoom   = False, 
    requires    = Nothing, 
    items       = [], 
    monsters    = [WoodTroll 10 Key], 
    doors       = [(West , startingRoom)], 
    actions     = attackAction
}

attackAction :: Item -> GameState -> Next GameState
attackAction Spoon (GS p r) =
    case monsters r of
        [] -> Same "There are no monsters left in this room"
        ((WoodTroll h i) : ms) ->
            if h > 5
                then
                    let r' = r {monsters = WoodTroll {health = h-5, holding = i} : ms}
                        p' = p {healthPoints = healthPoints p - 2} -- 3.17 BONUS 
                        in Progress "You deal 5 damage to the wood troll! *Bleed*. It is still alive.\nThe wood troll strikes back! *Oof*. It deals you 2 damage." (GS p' r')
                else 
                    let r' = r {monsters = ms, items = [(Key, "On the ground")]}
                        in Progress "You kill the wood troll! *Grrowr!*. It drops a key on the ground" (GS p r')              
        ((Vampire h i) : ms) -> 
            if h > 5
                then
                    let r' = r {monsters = Vampire {health = h-5, holding = i} : ms}
                        p' = p {healthPoints = healthPoints p - 1} 
                        in Progress "You deal 5 damage to the vampire! *Bleed*. It is still alive.\nThe vampire strikes back! *Oof*. It deals you 1 damage." (GS p' r')
                else 
                    let r' = r {monsters = ms, items = [(Jacket, "On the carpet")]}
                        in Progress "You kill the vampire! *Arghhh!*. It drops a jacket on the carpet" (GS p r')
attackAction item gs = noActions item gs

game0 :: GameState
game0 = GS {
    player = Player "Daniel" [] 5,
    room   = startingRoom
}

instance Parsable Item where
    parse "spoon"  = Just Spoon
    parse "key"    = Just Key
    parse "jacket" = Just Jacket
    parse _        = Nothing

instance Parsable Direction where
    parse "north" = Just North 
    parse "south" = Just South 
    parse "east"  = Just East 
    parse "west"  = Just West 
    parse _ = Nothing

instance Parsable Command where
    parse ('g' : 'o' : ' ' : dir) = case parse dir of 
        Just d -> Just (Move d)
        Nothing -> Nothing
    parse ('g' : 'r' : 'a' : 'b' : ' ' : item) = case parse item of
        Just i -> Just (PickUp i) 
        Nothing     -> Nothing
    parse ('u' : 's' : 'e' : ' ' : item) = case parse item of
        Just i -> Just (Use i)
        Nothing    -> Nothing
    parse "end" = Just End
    parse _     = Nothing

tellResponse :: String -> IO ()
tellResponse s = putStrLn $ "< " ++ s

readCommand :: IO (Maybe Command)
readCommand = do 
     putStr "> "
     input <- getLine 
     return (parse input)

deleteFrom :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom n [] = []
deleteFrom n ((x, y) : xs) = if x == n then xs else
    (x, y) : deleteFrom n xs
    
leaveRoom :: Room -> Direction -> Room -> Room
leaveRoom fr dir tr = tr {doors = (opposite dir, fr) : deleteFrom (opposite dir) (doors tr)}

step :: Command -> GameState -> Next GameState
step (Move dir) (GS p r) = case lookup dir (doors r) of
    Nothing   -> Same  "There are no doors in that direction"
    Just room -> case requires room of 
        Nothing   -> Progress ("You moved through the door to the " ++ show dir) (GS p (leaveRoom r dir room)) 
        Just item -> if item `elem` inventory p
                     then Progress ("You unlocked and went through the door to the " ++ show dir) (GS p (leaveRoom r dir room)) 
                     else Same  ("You don't have required item. This door requires a " ++ show item) 
step (PickUp item) (GS p r) = case lookup item (items r) of
    Nothing -> Same "There is no such item to pick up"    
    Just i  -> let r' = r {items = deleteFrom item (items r)}
                   p' = p {inventory = item : inventory p}
               in Progress ("You picked up the " ++ show item) (GS p' r')   
step (Use item) (GS p r) = if item `elem` inventory p 
                           then actions r item (GS p r)
                           else Same ("You don't possess such item as "++ show item)       
            
play :: GameState -> IO ()
play gs = do 
    tellContext gs
    playLoop gs

playLoop :: GameState -> IO ()
playLoop (GS p r) = if isWinRoom r
                    then tellResponse ("Congratulations " ++ playerName p ++ "! You've beaten the game. You rock!")
                    else do 
                        c <- readCommand 
                        case c of
                            Nothing -> do
                                putStrLn "Unknown command"
                                playLoop (GS p r)
                            Just End -> do
                                tellResponse  "You quit the game. Try again next time."
                                return ()
                            Just command -> case step command (GS p r) of
                                Same m -> do
                                    tellResponse m
                                    playLoop (GS p r)
                                Progress m ngs-> do
                                    tellResponse m
                                    play ngs
                    

main = do
    putStrLn "Welcome! You are about to experience a wild, text-based adventure. Good luck and have fun."
    play game0           

monsterRoom2 :: Room
monsterRoom2 = Room {
    name        = "Vampire's bedroom", 
    description = "warm and scarlet", 
    isWinRoom   = False, 
    requires    = Nothing, 
    items       = [], 
    monsters    = [Vampire 13 Jacket], 
    doors       = [(East , startingRoom), (South, bonusRoom)], 
    actions     = attackAction
}

bonusRoom :: Room
bonusRoom = Room {
    name        = "Ice Cave", 
    description = "extremely cold and chilly", 
    isWinRoom   = False, 
    requires    = Nothing, 
    items       = [], 
    monsters    = [], 
    doors       = [(North , monsterRoom2)], 
    actions     = equipAction
}

-- Remove item from player's inventory.
removeItem :: Eq t => t -> [t] -> [t]
removeItem i [] = []
removeItem i (x : xs) = if x == i then xs else 
    x : removeItem i xs

equipAction :: Item -> GameState -> Next GameState
equipAction Jacket (GS p r) = let p' = p {inventory = removeItem Jacket (inventory p)}
                              in Progress "You put the jacket on. You no longer feel cold." (GS p' r)    
equipAction item gs = noActions item gs
