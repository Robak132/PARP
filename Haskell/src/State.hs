module State where
    import Utilites ( printLines )
    import qualified System.Random as Random
    import Character (Character (location, name), basicSkeleton, basicGuardian, fallenCat, character)
    import Doors (Door (Door))
    data State = State {
            comment :: [String],
            you :: Character,
            items_at :: [(String, String)],
            enemies :: [Character],
            doors :: [Door],
            holding :: [String],
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
            ("Key", "attendant_room"),
            ("Torch", "acolyte_chamber_1"),
            ("Floating Crystal", "false_floor_room")
        ]
    -- Enemies
        [
            basicSkeleton {name = "Skele-cat",        location = "attendant_room"},
            basicGuardian {name = "Catmint Guardian", location = "guardian"},
            fallenCat     {name = "Fallen Cat",       location = "sarcophagus"}
        ]
    -- Doors
        [
            Door "Gold Door" "acolyte_chamber_2" "serket_chamber" False,
            Door "Moonlight Door" "acolyte_chamber_2" "acolyte_chamber_1" False
        ]
    -- Holding
        []
    -- Random generator
        (Random.mkStdGen 2137)
