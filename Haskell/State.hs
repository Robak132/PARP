module State where
    import Utilites ( printLines )
    data State = State {
            comment :: [String],
            i_am_at :: String,
            items_at :: [(String, String)],
            enemy_at :: [(String, String)],
            health :: [(String, Integer)],
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
    -- My location
        "entrance"
    -- Items
        [
            ("Key", "attendant_room"),
            ("Torch", "acolyte_chamber_1"),
            ("Floating Crystal", "false_floor_room")
        ]
    -- Enemies
        [
            ("skele_cat_1", "attendant_room"),
            ("catmint_guardian", "guardian"),
            ("fallen_cat", "sarcophagus")
        ]
    -- Health
        [
            ("player", 6),
            ("skele_cat_1", 3),
            ("catmint_guardian", 13),
            ("fallen_cat", 6)
        ]
    -- Doors
        [
            ("acolyte_chamber_2", "serket_chamber", "normal_door", False),
            ("acolyte_chamber_2", "acolyte_chamber_1", "moonlight_door", False)
        ]
    -- Holding
        [
        ]
