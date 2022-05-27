module Traps where
    data Trap = Trap {
        name :: String,
        location_from :: String,
        location_to :: String,
        damage :: Int,
        dodge :: Int
    } deriving (Show)

    bladeTrap :: Trap
    bladeTrap = Trap "Falling Blade" "false_floor_room" "trap_corridor" 6 10

    slabTrap :: Trap
    slabTrap = Trap "Fake Slab" "trap_corridor" "treasure_room" 2 6
