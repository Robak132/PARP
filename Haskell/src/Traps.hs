module Traps where
    data Trap = Trap {
        location_from :: String,
        location_to :: String,
        strengthModifier :: Int,
        damage :: Int,
        dodge :: Int
    } deriving (Show)

    bladeTrap :: Trap
    bladeTrap = Trap "false_floor_room" "trap_corridor_a" 0 6 10

    slabTrap :: Trap
    slabTrap = Trap "trap_corridor_b" "treasure_room" 0 2 6
