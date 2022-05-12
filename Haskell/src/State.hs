module State where
    import qualified System.Random as Random
    import Utilites ( printLines )
    import Character (Character (location, name), basicSkeleton, basicGuardian, fallenCat, character)
    import Doors (Door, goldDoor, moonlightDoor)
    import Items (Item, key, torch, floatingCrystal)
    import Traps (Trap, bladeTrap, tripWire, dustTrap)

    data State = State {
            comment :: [String],
            you :: Character,
            items :: [Item],
            enemies :: [Character],
            doors :: [Door],
            traps :: [Trap],
            holding :: [Item],
            randomGen :: Random.StdGen 
        } deriving (Show)

    printState :: State -> IO ()
    printState state = printLines(comment state)

    randInt :: (Int, Int) -> State -> (Int, State)
    randInt a state = do 
        let (rand, gen) = Random.randomR a (randomGen state)
        (rand, state {randomGen=gen})

    initialState :: State
    initialState = State
    -- Comment
        []
    -- You
        character {location = "entrance"}
    -- Items
        [
            key,
            torch,
            floatingCrystal
        ]
    -- Enemies
        [
            basicSkeleton {name = "Skele-cat",        location = "attendant_room"},
            basicGuardian {name = "Catmint Guardian", location = "guardian"},
            fallenCat     {name = "Fallen Cat",       location = "sarcophagus"}
        ]
    -- Doors
        [
            goldDoor,
            moonlightDoor
        ]
    -- Traps
        [
            bladeTrap,
            tripWire,
            dustTrap
        ]
    -- Holding
        []
    -- Random generator
        (Random.mkStdGen 2137)
