module State where
    import Utilites ( printLines )
    import Character (Character (Character, location, name), basicSkeleton, basicGuardian, fallenCat, character)
    data State = State {
            comment :: [String],
            you :: Character,
            items_at :: [(String, String)],
            enemies :: [Character],
            door :: [(String, String, String, Bool)],
            holding :: [String]
        } deriving (Show)

    printState :: State -> IO ()
    printState state = printLines(comment state)

    initialState :: State
    initialState = State
    -- Comment
        [
        ]
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
            basicSkeleton {name = "Skale-cat",        location = "attendant_room"},
            basicGuardian {name = "Catmint Guardian", location = "guardian"},
            fallenCat     {name = "Fallen Cat",       location = "sarcophagus"}
        ]
    -- Doors
        [
            ("acolyte_chamber_2", "serket_chamber", "normal_door", False),
            ("acolyte_chamber_2", "acolyte_chamber_1", "moonlight_door", False)
        ]
    -- Holding
        [
        ]
