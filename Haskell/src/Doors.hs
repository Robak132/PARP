module Doors where
    data Door = Door {
        name :: String,
        location_from :: String,
        location_to :: String,
        status :: Bool
    } deriving (Show, Eq)

    goldDoor :: Door
    goldDoor = Door "Gold Door" "acolyte_chamber_2" "serket_chamber" False
    
    moonlightDoor :: Door
    moonlightDoor = Door "Moonlight Door" "acolyte_chamber_2" "acolyte_chamber_1" False