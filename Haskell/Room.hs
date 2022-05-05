module Room where
    import qualified Data.List as List
    import State ( State(comment, i_am_at) )

    data Direction = N | W | E | S deriving (Read, Show, Enum, Eq)

    data RoomConnection = RoomConnection {
        from :: String,
        by :: Direction,
        to :: String
        } deriving (Show)

    connections :: [RoomConnection]
    connections = [
        RoomConnection "entrance" N "antechamber",
        RoomConnection "antechamber" N "altar_room",
        RoomConnection "antechamber" S "entrance",
        RoomConnection "antechamber" W "jar_room",
        RoomConnection "antechamber" E "attendant_room",
        RoomConnection "jar_room" E "antechamber",
        RoomConnection "jar_room" W "acolyte_chamber_1",
        RoomConnection "attendant_room" W "antechamber",
        RoomConnection "attendant_room" N "corridor",
        RoomConnection "corridor" N "false_floor_room",
        RoomConnection "corridor" W "altar_room",
        RoomConnection "acolyte_chamber_1" N "acolyte_chamber_2",
        RoomConnection "acolyte_chamber_1" E "jar_room",
        RoomConnection "acolyte_chamber_2" S "acolyte_chamber_1",
        RoomConnection "acolyte_chamber_2" E "serket_chamber",
        RoomConnection "altar_room" S "antechamber",
        RoomConnection "altar_room" E "corridor",
        RoomConnection "false_floor_room" S "corridor",
        RoomConnection "false_floor_room" N "trap_corridor",
        RoomConnection "trap_corridor" S "false_floor_room",
        RoomConnection "trap_corridor" N "treasure_room",
        RoomConnection "serket_chamber" W "acolyte_chamber_2",
        RoomConnection "serket_chamber" N "guardian",
        RoomConnection "guardian" S "serket_chamber",
        RoomConnection "guardian" N "sarcophagus",
        RoomConnection "treasure_room" S "trap_corridor",
        RoomConnection "treasure_room" N "hidden_exit",
        RoomConnection "sarcophagus" S "guardian",
        RoomConnection "sarcophagus" E "hidden_exit",
        RoomConnection "hidden_exit" N "sarcophagus",
        RoomConnection "hidden_exit" S "treasure_room"
        ]

    data RoomDescription = RoomDescription {
        name :: String,
        description :: [String]
        } deriving (Show)

    descriptions :: [RoomDescription]
    descriptions = [
        RoomDescription "entrance" ["You are at the entrance to tomb. There is an gate before you, with small cat door"],
        RoomDescription "attendant_room" ["You are in a room filled with sceletons."],
        RoomDescription "antechamber" ["You are in the first room. The walls are covered in hieroglyphic description of the antient curse that forbids any cat that walks in there to go to heaven. They will be forever doomed to live in the tomb, turned into skeletons."],
        RoomDescription "jar_room" ["You have entered the romm filled with jars. There are some tasty bones and shiny jewels in them"],
        RoomDescription "corridor" ["You are in the dark corridor."],
        RoomDescription "acolyte_chamber_1" ["You are in yet another room. You see door with a symbol of the moon and long shadows."],
        RoomDescription "acolyte_chamber_2" ["You see tombs of important cats. Unfortunately cats can\'t write, so you don\'t know their names."],
        RoomDescription "altar_room" ["You walked to the room with big altar in the middle."],
        RoomDescription "false_floor_room" ["The centre of the room has a marble table with a floating purple crystal. The floor in the middle looks cracked and hastily built."],
        RoomDescription "trap_corridor" ["You have entered yet another dark corridor, but this one looks scarier."],
        RoomDescription "serket_chamber" ["The hieroglyphs in this room describe how every cat devotes their life to lasagna, and therefore is cursed dou to its greed"],
        RoomDescription "guardian" ["You are in the room lit with hundreds of candles. In the middle there is a guardian, chained to a metal pole"],
        RoomDescription "treasure_room" ["There is a variety of treasure, such as bones and tennis balls. There is also some ancient stuff"],
        RoomDescription "sarcophagus" ["There is a big sarcophagus in the middle of the room"],
        RoomDescription "hidden_exit" ["There are two statues of cats in this room. Under one of them a small breeze can be felt.", "You made it to the end, please enter the 'quit' command."]
        ]

    go :: Direction -> State -> State
    go direction state = do
        case List.find (\x -> from x == i_am_at state && by x == direction) connections of
            Nothing -> state { comment = ["There is no way there."]}
            Just room -> look(state {i_am_at = to room})

    look :: State -> State
    look state = do
        case List.find (\x-> name x == i_am_at state) descriptions of
            Nothing -> state { comment = ["There is nothing here, probably an error"]}
            Just desc -> state { comment = description desc }
