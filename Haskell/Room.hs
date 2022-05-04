module Room where
    import qualified Data.List as List
    import State
    import Utilites

    data Direction = N | W | E | S deriving (Read, Show, Enum, Eq)

    data RoomConnection = RoomConnection {
        from :: String,
        by :: Direction,
        to :: String
        } deriving (Show)
    
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

    go direction state = do
        case (List.find (\(x) -> from x == i_am_at state && by x == direction) connections) of
            Nothing -> state
            Just(room) -> state { i_am_at = to room, comment = [to room] }