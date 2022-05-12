module State where
    import Utilites ( printLines )
    import qualified System.Random as Random
    import Character (Character (location, name), basicSkeleton, basicGuardian, fallenCat, character)
    import Doors (Door, goldDoor, moonlightDoor)
    import Items (Item, key, torch, floatingCrystal)

    data State = State {
            comment :: [String],
            you :: Character,
            items :: [Item],
            enemies :: [Character],
            doors :: [Door],
            holding :: [Item],
            randomGen :: Random.StdGen 
        } deriving (Show)

    printState :: State -> IO ()
    printState state = printLines(comment state)

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
    -- Holding
        []
    -- Random generator
        (Random.mkStdGen 2137)
