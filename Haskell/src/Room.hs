module Room where
    import qualified Data.List as List
    import State (State(comment, you, items, enemies, doors, traps, holding), randInt)
    import Character (Character(name, location, health), alive)
    import Combat (harm)
    import Doors (Door (location_from, location_to, status, name), moonlightDoor, goldDoor)
    import Traps (Trap (location_from, location_to, dodge, damage))
    import Items (Item(Item, itemLocation, name), torch, key)

    data RoomConnection = RoomConnection {
        from :: String,
        by :: String,
        to :: String
        } deriving (Show)

    connections :: [RoomConnection]
    connections = [
        RoomConnection "entrance" "N" "antechamber",
        RoomConnection "antechamber" "N" "altar_room",
        RoomConnection "antechamber" "S" "entrance",
        RoomConnection "antechamber" "W" "jar_room",
        RoomConnection "antechamber" "E" "attendant_room",
        RoomConnection "jar_room" "E" "antechamber",
        RoomConnection "jar_room" "W" "acolyte_chamber_1",
        RoomConnection "attendant_room" "W" "antechamber",
        RoomConnection "attendant_room" "N" "corridor",
        RoomConnection "corridor" "N" "false_floor_room",
        RoomConnection "corridor" "W" "altar_room",
        RoomConnection "acolyte_chamber_1" "N" "acolyte_chamber_2",
        RoomConnection "acolyte_chamber_1" "E" "jar_room",
        RoomConnection "acolyte_chamber_2" "S" "acolyte_chamber_1",
        RoomConnection "acolyte_chamber_2" "E" "serket_chamber",
        RoomConnection "altar_room" "S" "antechamber",
        RoomConnection "altar_room" "E" "corridor",
        RoomConnection "false_floor_room" "S" "corridor",
        RoomConnection "false_floor_room" "N" "trap_corridor_a",
        RoomConnection "trap_corridor_a" "S" "false_floor_room",
        RoomConnection "trap_corridor_a" "N" "trap_corridor_b",
        RoomConnection "trap_corridor_b" "S" "trap_corridor_a",
        RoomConnection "trap_corridor_b" "N" "treasure_room",
        RoomConnection "serket_chamber" "W" "acolyte_chamber_2",
        RoomConnection "serket_chamber" "N" "guardian",
        RoomConnection "guardian" "S" "serket_chamber",
        RoomConnection "guardian" "N" "sarcophagus",
        RoomConnection "treasure_room" "S" "trap_corridor_c",
        RoomConnection "treasure_room" "N" "hidden_exit",
        RoomConnection "sarcophagus" "S" "guardian",
        RoomConnection "sarcophagus" "E" "hidden_exit",
        RoomConnection "hidden_exit" "N" "sarcophagus",
        RoomConnection "hidden_exit" "S" "treasure_room"
        ]

    data RoomDescription = RoomDescription {
        name :: String,
        description :: [String]
        } deriving (Show)

    descriptions :: [RoomDescription]
    descriptions = [
        RoomDescription "entrance" ["You are at the entrance to tomb. There is an gate before you, with small cat door"],
        RoomDescription "attendant_room" ["You are in a room filled with skeletons."],
        RoomDescription "antechamber" ["You are in the first room. The walls are covered in hieroglyphic description of the antient curse that forbids any cat that walks in there to go to heaven. They will be forever doomed to live in the tomb, turned into skeletons."],
        RoomDescription "jar_room" ["You have entered the romm filled with jars. There are some tasty bones and shiny jewels in them"],
        RoomDescription "corridor" ["You are in the dark corridor."],
        RoomDescription "acolyte_chamber_1" ["You are in yet another room. You see door with a symbol of the moon and long shadows."],
        RoomDescription "acolyte_chamber_2" ["You see tombs of important cats. Unfortunately cats can\'t read, so you don\'t know their names."],
        RoomDescription "altar_room" ["You walked to the room with big altar in the middle."],
        RoomDescription "false_floor_room" ["The centre of the room has a marble table with a floating purple crystal. The floor in the middle looks cracked and hastily built. You see massive blades falling from the roof and reseting after that on the north."],
        RoomDescription "trap_corridor_a" ["You have entered yet another dark corridor. You see massive blades falling from the roof and reseting after that on your south."],
        RoomDescription "trap_corridor_b" ["You are going further through corridor, one of the slabs on the north looks suspiciously."],
        RoomDescription "serket_chamber" ["The hieroglyphs in this room describe how every cat devotes their life to lasagna, and therefore is cursed dou to its greed"],
        RoomDescription "guardian" ["You are in the room lit with hundreds of candles. In the middle there is a guardian, chained to a metal pole"],
        RoomDescription "treasure_room" ["There is a variety of treasure, such as bones and tennis balls. There is also some ancient stuff, one of the slabs on the south of you looks suspiciously."],
        RoomDescription "sarcophagus" ["There is a big sarcophagus in the middle of the room"],
        RoomDescription "hidden_exit" ["There are two statues of cats in this room. Under one of them a small breeze can be felt.", "You made it to the end, please enter the 'quit' command."]
        ]

    go :: String -> State -> State
    go direction state = case List.find (\x -> from x == location (you state) && by x == direction) connections of
        Nothing -> state { comment = ["There is no way there."]}
        Just room -> case List.find (\x -> location (you state) == location x) (enemies state) of
            Nothing -> checkDoors room state
            Just enemy -> if not (alive enemy) then checkDoors room state else state { comment = ["You cannot exit room, when is monster in it."]}

    checkTraps :: RoomConnection -> State -> State
    checkTraps room state = case List.find (\x -> from room == Traps.location_from x && to room == Traps.location_to x || from room == Traps.location_to x && to room == Traps.location_from x) (traps state) of
        Nothing -> look state
        Just trap -> do
            let (dodgeRoll, modifiedState) = randInt (1, 20) state
            if dodgeRoll >= dodge trap then do
                let (damageRoll, modifiedState2) = randInt (1, Traps.damage trap) modifiedState
                let (modifiedCharacter, modifiedState3) = harm (you modifiedState2) damageRoll modifiedState2

                lookAdd modifiedState3 {comment=["Trap was trigerred [" ++ show damageRoll ++ " dmg]. Remaining HP " ++ show (health modifiedCharacter)], you = ((you modifiedState3) {location = to room})}
            else
                lookAdd modifiedState {comment=["Trap wasn't trigerred"], you = ((you modifiedState) {location = to room})}

    checkDoors :: RoomConnection -> State -> State
    checkDoors room state = case List.find (\x -> from room == Doors.location_from x && to room == Doors.location_to x || from room == Doors.location_to x && to room == Doors.location_from x) (doors state)  of
        Nothing -> checkTraps room (state {you = (you state) {location = to room}})
        Just door | door == moonlightDoor &&
                    notElem torch {itemLocation="acolyte_chamber_1"} (items state) &&
                    notElem torch {itemLocation="acolyte_chamber_2"} (items state) &&
                    notElem torch {itemLocation=""} (holding state) -> checkTraps room (state {you = (you state) {location = to room}})
                  | door == goldDoor &&
                    elem key {itemLocation=""} (holding state) -> checkTraps room (state {comment = ["You opened Gold Door using Key."], you = (you state) {location = to room}, doors = List.delete door (doors state)})
                  | status door -> checkTraps room (state {you = (you state) {location = to room}})
                  | otherwise -> lookAdd(state {comment = ["You tried to open " ++ Doors.name door ++ " but is locked"]})

    flee :: String -> State -> State
    flee direction state = case List.find (\x -> from x == location (you state) && by x == direction) connections of
        Nothing -> state { comment = ["There is no way there."]}
        Just room -> case List.find (\x -> location (you state) == location x) (enemies state) of
            Nothing -> checkDoors room state
            Just enemy -> if alive enemy then do
                    let (_, modifiedState) = harm (you state) 1 state {comment = []}
                    lookAdd (modifiedState {you = (you modifiedState) {location = to room}})
                else
                    checkDoors room state

    lookAdd :: State -> State
    lookAdd state = case List.find (\x-> Room.name x == location (you state)) descriptions of
        Nothing -> findExits state { comment = comment state ++ ["There is nothing here, probably an error."]}
        Just desc -> findExits state { comment = comment state ++ description desc }

    look :: State -> State
    look state = case List.find (\x-> Room.name x == location (you state)) descriptions of
        Nothing -> findExits state { comment = ["There is nothing here, probably an error."]}
        Just desc -> findExits state { comment = description desc }

    findExits :: State -> State
    findExits state = case map by (filter (\x -> from x == location (you state)) connections) of
        [] -> findEnemies state
        directions -> findEnemies state { comment = comment state ++ ["You may go from here to: " ++ List.intercalate ", " directions] }

    findEnemies :: State -> State
    findEnemies state = case List.find (\x -> location (you state) == location x) (enemies state) of
        Nothing -> state
        Just enemy ->
            if alive enemy then
                state { comment = comment state ++ ["There is " ++ Character.name enemy ++ " here. Time to fight!"] }
            else
                state